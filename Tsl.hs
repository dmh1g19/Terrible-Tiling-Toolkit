{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BangPatterns #-}

module Tsl
  ( runTslInBrowser
  ) where

import Tokens
import Grammar
import Evaluator
import System.IO
import Control.Exception
import qualified Data.Map.Strict as Map

-- compile normally and then pipe the contents in, like so:
-- ghc Tsl.hs -o Tsl
-- ./Tsl < pr1.tails
-- and have the .tl (tile files) in the same directory

-- | Original main for command-line usage (optional).
main :: IO ()
main = catch main' noParse

main' :: IO ()
main' = do
  sourceText <- do
    contents <- getContents
    if null contents
      then error "No TSL program on stdin. Provide a .tsl or pipe input."
      else return contents
  let lexedProg  = alexScanTokens sourceText
  let parsedProg = parseCalc lexedProg
  let expList    = flattenMultiExpr parsedProg []
  let (tileFiles, otherExps) = extractTileFiles expList
  initialEnv <- loadTileFiles tileFiles emptyEnv
  let (_, _, revOutput) = evaluateExp (otherExps, initialEnv, [])
  mapM_ putStrLn (reverse revOutput)

noParse :: ErrorCall -> IO ()
noParse e = do
  hPutStrLn stderr (show e)
  return ()

-- | Empty environment using Maps
emptyEnv :: Environment
emptyEnv = (Map.empty, Map.empty)

-- | Run TSL purely in memory (no disk IO).
runTslInBrowser
  :: String                   -- ^ TSL source code
  -> [(String, String)]       -- ^ A lookup table of tile filenames -> tile text
  -> [String]                 -- ^ The output lines from the TSL program
runTslInBrowser tslCode tileData =
  let !lexedProg  = alexScanTokens tslCode
      !parsedProg = parseCalc lexedProg
      !expList    = flattenMultiExpr parsedProg []
      !(tileFiles, exps) = extractTileFiles expList
      !env0    = loadTileFilesInMemory tileFiles tileData emptyEnv
      !(_, _, revOutput) = evaluateExp (exps, env0, [])
  in reverse revOutput

-- | In-memory tile loader — lookup filenames in tileData
loadTileFilesInMemory
  :: [(String, String)]   -- ^ (filename, varname) from TSL
  -> [(String, String)]   -- ^ tileData with (filename, tile content)
  -> Environment
  -> Environment
loadTileFilesInMemory [] _ env = env
loadTileFilesInMemory ((filename, varname):rest) tileData env =
  case lookup filename tileData of
    Nothing ->
      error $ "Tile file not found in memory: " ++ filename
    Just content ->
      let !tile   = Tile (words content)
          !newEnv = addTileVar varname tile env
      in  loadTileFilesInMemory rest tileData newEnv

--------------------------------------------------------------------------------
-- Flattening and extraction — using accumulator to avoid O(n^2) from (++)
--------------------------------------------------------------------------------

flattenMultiExpr :: Exp -> [Exp] -> [Exp]
flattenMultiExpr (MultiExpr e1 e2) acc =
  flattenMultiExpr e1 (flattenMultiExpr e2 acc)
flattenMultiExpr e acc = e : acc

extractTileFiles :: [Exp] -> ([(String, String)], [Exp])
extractTileFiles [] = ([], [])
extractTileFiles (ExpGetTileFile file name : xs) =
  let (files, rest) = extractTileFiles xs
  in ((file, name) : files, rest)
extractTileFiles (e : xs) =
  let (files, rest) = extractTileFiles xs
  in (files, e : rest)

-- | Disk-based loader for CLI usage
loadTileFiles :: [(String, String)] -> Environment -> IO Environment
loadTileFiles [] env = return env
loadTileFiles ((filename, varname) : rest) env = do
  content <- readFile (filename ++ ".tl")
  let !tile   = Tile (words content)
  let !newEnv = addTileVar varname tile env
  loadTileFiles rest newEnv
{-# LANGUAGE OverloadedStrings #-}

module Tsl 
  ( runTslInBrowser  -- export this for the browser
  ) where

import Tokens
import Grammar
import Evaluator
import System.IO
import Control.Exception

-- compile normally and then pipe the contents in, like so:
-- ghc Tsl.hs -o Tsl
-- ./Tsl < pr1.tails
-- and have the .tl (tile files) in the same directory

-- | Original main for command-line usage (optional).
--   You can keep it if you still want to compile/run in a normal GHC environment.
main :: IO ()
main = catch main' noParse

main' :: IO ()
main' = do
  -- This part still expects a .tsl filename from the command line:
  --     runghc Tsl.hs myprogram.tsl
  sourceText <- do
    contents <- getContents
    if null contents
      then error "No TSL program on stdin. Provide a .tsl or pipe input."
      else return contents
  let lexedProg  = alexScanTokens sourceText
  let parsedProg = parseCalc lexedProg
  let expList    = flattenMultiExpr parsedProg
  let (tileFiles, otherExps) = extractTileFiles expList

  -- Load tile files from local disk (old style)
  initialEnv <- loadTileFiles tileFiles ([], [])
  let (_, _, output) = evaluateExp (otherExps, initialEnv, [])
  mapM_ putStrLn output

noParse :: ErrorCall -> IO ()
noParse e = do
  hPutStrLn stderr (show e)
  return ()

-- | New function: run TSL purely in memory (no disk IO).
--   Supply the TSL source plus a list of (fileName, tileContent) pairs.
--   Returns all output lines (e.g. printed tiles) as a pure list of Strings.
runTslInBrowser
  :: String                   -- ^ TSL source code
  -> [(String, String)]       -- ^ A lookup table of tile filenames -> tile text
  -> [String]                 -- ^ The output lines from the TSL program
runTslInBrowser tslCode tileData =
  let lexedProg       = alexScanTokens tslCode
      parsedProg      = parseCalc lexedProg
      expList         = flattenMultiExpr parsedProg
      (tileFiles, exps) = extractTileFiles expList

      -- We do "IO" in loadTileFilesInMemory, but it is effectively pure
      -- (unless you adapt it to do asynchronous browser fetches, etc.)
      (finalEnv, output) =
        let env0 = ([], [])
            (_, env1, out) = evaluateExp (exps, loadTileFilesInMemory tileFiles tileData env0, [])
        in (env1, out)
  in output

-- | A purely in-memory version of loadTileFiles.
--   Instead of 'readFile', we do a lookup in tileData.
loadTileFilesInMemory
  :: [(String, String)]   -- ^ (filename, varname) pairs extracted from TSL
  -> [(String, String)]   -- ^ tileData with (filename, tile content)
  -> Environment          -- ^ initial environment
  -> Environment          -- ^ final environment
loadTileFilesInMemory [] _ env = env
loadTileFilesInMemory ((filename, varname):rest) tileData env =
  case lookup filename tileData of
    Nothing ->
      error $ "Tile file not found in memory: " ++ filename
    Just content ->
      let tile   = Tile (words content)
          newEnv = addTileVar varname tile env
      in  loadTileFilesInMemory rest tileData newEnv

--------------------------------------------------------------------------------
-- Existing code unchanged below here, except you may want them in same module --
--------------------------------------------------------------------------------

flattenMultiExpr :: Exp -> [Exp]
flattenMultiExpr (MultiExpr e1 e2) = flattenMultiExpr e1 ++ flattenMultiExpr e2
flattenMultiExpr e = [e]

extractTileFiles :: [Exp] -> ([(String, String)], [Exp])
extractTileFiles [] = ([], [])
extractTileFiles (ExpGetTileFile file name : xs) =
  let (files, rest) = extractTileFiles xs
   in ((file, name) : files, rest)
extractTileFiles (e : xs) =
  let (files, rest) = extractTileFiles xs
   in (files, e : rest)

-- | Original disk-based loader (used by main for local CLI usage)
loadTileFiles :: [(String, String)] -> Environment -> IO Environment
loadTileFiles [] env = return env
loadTileFiles ((filename, varname) : rest) env = do
  content <- readFile (filename ++ ".tl")
  let tile   = Tile (words content)
  let newEnv = addTileVar varname tile env
  loadTileFiles rest newEnv

{-# LANGUAGE BangPatterns #-}
module Evaluator where

import Control.DeepSeq
import Data.List (intercalate, transpose)
import qualified Data.Map.Strict as Map
import Grammar

-- Use strict Maps for O(log n) lookup/insert instead of O(n) association lists
type TileEnv = Map.Map String TileVar
type IntEnv  = Map.Map String Int
type Environment = (TileEnv, IntEnv)

-- State includes expressions, environment, and accumulated output (reversed)
type State = ([Exp], Environment, [String])

data TileVar = Tile ![String]
  deriving (Show, Eq)

instance NFData TileVar where
  rnf (Tile xs) = rnf xs

-- Evaluate expressions and return final state with output
evaluateExp :: State -> State
evaluateExp state@([], _, _) = state

evaluateExp ((ExpReturn tile1) : xs, env, output) =
  let !(newTile1, env') = evaluateExpTile (tile1, env)
  in (xs, env', prettyPrint newTile1 : output)

evaluateExp (ExpDoNothing : xs, env, output) =
  evaluateExp (xs, env, output)

evaluateExp ((MultiExpr exp1 exp2) : xs, env, output) =
  evaluateExp (exp1 : exp2 : xs, env, output)

evaluateExp ((ExpIf bool1 exp1 exp2) : xs, env, output)
  | evaluateExpBool (bool1, env) = evaluateExp (exp1 : xs, env, output)
  | otherwise                    = evaluateExp (exp2 : xs, env, output)

evaluateExp ((ExpWhile bool1 exp1) : xs, env, output)
  | evaluateExpBool (bool1, env) =
      evaluateExp (exp1 : ExpWhile bool1 exp1 : xs, env, output)
  | otherwise = evaluateExp (xs, env, output)

evaluateExp ((ExpPrint tile1) : xs, env, output) =
  let !(newTile1, env') = evaluateExpTile (tile1, env)
  in evaluateExp (xs, env', prettyPrint newTile1 : output)

evaluateExp ((ExpSetTileVar name tile1) : xs, env, output) =
  let !(newTile1, env') = evaluateExpTile (tile1, env)
      !env'' = addTileVar name newTile1 env'
  in evaluateExp (xs, env'', output)

evaluateExp ((ExpSetIntVar name int1) : xs, env, output) =
  let !newInt1 = evaluateExpInt (int1, env)
      !env' = addIntVar name newInt1 env
  in evaluateExp (xs, env', output)

-- ---------------------------------------------------------------------------
-- Environment operations — O(log n) with Data.Map.Strict
-- ---------------------------------------------------------------------------

getTileVar :: String -> Environment -> TileVar
getTileVar name (tiles, _) =
  case Map.lookup name tiles of
    Just tile -> tile
    Nothing   -> error $ "Tile variable not found: " ++ name

addTileVar :: String -> TileVar -> Environment -> Environment
addTileVar name !tile (tiles, ints) =
  (Map.insert name tile tiles, ints)

addIntVar :: String -> Int -> Environment -> Environment
addIntVar name !val (tiles, ints) =
  (tiles, Map.insert name val ints)

getIntVar :: String -> Environment -> Int
getIntVar name (_, ints) =
  case Map.lookup name ints of
    Just val -> val
    Nothing  -> error $ "Error: Variable name for integers: "
                     ++ name ++ " does not exist"

-- ---------------------------------------------------------------------------
-- Tile expression evaluation
-- ---------------------------------------------------------------------------

-- | Force the tile result deeply to avoid thunk chains in GHCJS.
--   Without this, each operation wraps its result in lazy thunks that
--   pile up and get forced all at once later — very slow under GHCJS.
strictResult :: TileVar -> Environment -> (TileVar, Environment)
strictResult !t !e = force t `seq` (t, e)
{-# INLINE strictResult #-}

evaluateExpTile :: (ExpTile, Environment) -> (TileVar, Environment)

evaluateExpTile (TileVar name, env) =
  (getTileVar name env, env)

evaluateExpTile (TileCTR tile1 tile2, env) =
  let !(t1, env1) = evaluateExpTile (tile1, env)
      !(t2, env2) = evaluateExpTile (tile2, env1)
  in strictResult (combineTilesRight t1 t2) env2

evaluateExpTile (TileCTD tile1 tile2, env) =
  let !(t1, env1) = evaluateExpTile (tile1, env)
      !(t2, env2) = evaluateExpTile (tile2, env1)
  in strictResult (combineTilesDown t1 t2) env2

evaluateExpTile (TileDTR tile1 int1, env) =
  let !(t1, env1) = evaluateExpTile (tile1, env)
      !n = evaluateExpInt (int1, env1)
  in strictResult (duplicateTileRight t1 n) env1

evaluateExpTile (TileDTD tile1 int1, env) =
  let !(t1, env1) = evaluateExpTile (tile1, env)
      !n = evaluateExpInt (int1, env1)
  in strictResult (duplicateTileDown t1 n) env1

evaluateExpTile (TileRT90 tile1, env) =
  let !(t1, env1) = evaluateExpTile (tile1, env)
  in strictResult (rotateTile90 t1) env1

evaluateExpTile (TileRT180 tile1, env) =
  let !(t1, env1) = evaluateExpTile (tile1, env)
  in strictResult (rotateTile180 t1) env1

evaluateExpTile (TileRT270 tile1, env) =
  let !(t1, env1) = evaluateExpTile (tile1, env)
  in strictResult (rotateTile270 t1) env1

evaluateExpTile (TileSRT tile1, env) =
  let !(t1, env1) = evaluateExpTile (tile1, env)
  in strictResult (squareRotateTile t1) env1

evaluateExpTile (TileST tile1 int1, env) =
  let !(t1, env1) = evaluateExpTile (tile1, env)
      !n = evaluateExpInt (int1, env1)
  in strictResult (scaleTile t1 n) env1

evaluateExpTile (TileCBT tile1, env) =
  let !(t1, env1) = evaluateExpTile (tile1, env)
  in strictResult (createBlankTile t1) env1

evaluateExpTile (TileRTX tile1, env) =
  let !(t1, env1) = evaluateExpTile (tile1, env)
  in strictResult (reflectTileX t1) env1

evaluateExpTile (TileRTY tile1, env) =
  let !(t1, env1) = evaluateExpTile (tile1, env)
  in strictResult (reflectTileY t1) env1

evaluateExpTile (TileRTXY tile1, env) =
  let !(t1, env1) = evaluateExpTile (tile1, env)
  in strictResult (reflectTileXY t1) env1

evaluateExpTile (TileSub tile1 i1 i2 i3 i4, env) =
  let !(t1, env1) = evaluateExpTile (tile1, env)
      !a = evaluateExpInt (i1, env1)
      !b = evaluateExpInt (i2, env1)
      !c = evaluateExpInt (i3, env1)
      !d = evaluateExpInt (i4, env1)
  in strictResult (createSubTile t1 a b c d) env1

evaluateExpTile (TileConjunct tile1 tile2, env) =
  let !(t1, env1) = evaluateExpTile (tile1, env)
      !(t2, env2) = evaluateExpTile (tile2, env1)
  in strictResult (conjunctTiles t1 t2) env2

evaluateExpTile (TileXor tile1 tile2, env) =
  let !(t1, env1) = evaluateExpTile (tile1, env)
      !(t2, env2) = evaluateExpTile (tile2, env1)
  in strictResult (xorTiles t1 t2) env2

evaluateExpTile (TileNegate tile1, env) =
  let !(t1, env1) = evaluateExpTile (tile1, env)
  in strictResult (negateTile t1) env1

evaluateExpTile (TileRemoveTop tile1 int1, env) =
  let !(t1, env1) = evaluateExpTile (tile1, env)
      !n = evaluateExpInt (int1, env1)
  in strictResult (removeTop t1 n) env1

-- ---------------------------------------------------------------------------
-- Integer expression evaluation
-- ---------------------------------------------------------------------------

evaluateExpInt :: (ExpInt, Environment) -> Int
evaluateExpInt (IntVal n, _)         = n
evaluateExpInt (IntVar name, env)    = getIntVar name env
evaluateExpInt (IntNegate i1, env)   = negate $! evaluateExpInt (i1, env)
evaluateExpInt (IntPlus i1 i2, env)  = evaluateExpInt (i1, env) + evaluateExpInt (i2, env)
evaluateExpInt (IntMinus i1 i2, env) = evaluateExpInt (i1, env) - evaluateExpInt (i2, env)
evaluateExpInt (IntTimes i1 i2, env) = evaluateExpInt (i1, env) * evaluateExpInt (i2, env)
evaluateExpInt (IntDivide i1 i2, env)= evaluateExpInt (i1, env) `div` evaluateExpInt (i2, env)
evaluateExpInt (IntExponential i1 i2, env) =
  evaluateExpInt (i1, env) ^ evaluateExpInt (i2, env)

-- ---------------------------------------------------------------------------
-- Boolean expression evaluation
-- ---------------------------------------------------------------------------

evaluateExpBool :: (ExpBool, Environment) -> Bool
evaluateExpBool (BoolTrue, _)  = True
evaluateExpBool (BoolFalse, _) = False
evaluateExpBool (BoolAnd b1 b2, env) =
  evaluateExpBool (b1, env) && evaluateExpBool (b2, env)
evaluateExpBool (BoolOr b1 b2, env) =
  evaluateExpBool (b1, env) || evaluateExpBool (b2, env)
evaluateExpBool (BoolNot b1, env) =
  not (evaluateExpBool (b1, env))
evaluateExpBool (BoolLessThan e1 e2, env) =
  evaluateExpInt (e1, env) < evaluateExpInt (e2, env)
evaluateExpBool (BoolLessEqualThan e1 e2, env) =
  evaluateExpInt (e1, env) <= evaluateExpInt (e2, env)
evaluateExpBool (BoolMoreThan e1 e2, env) =
  evaluateExpInt (e1, env) > evaluateExpInt (e2, env)
evaluateExpBool (BoolMoreEqualThan e1 e2, env) =
  evaluateExpInt (e1, env) >= evaluateExpInt (e2, env)
evaluateExpBool (BoolEqual e1 e2, env) =
  evaluateExpInt (e1, env) == evaluateExpInt (e2, env)
evaluateExpBool (BoolNotEqual e1 e2, env) =
  evaluateExpInt (e1, env) /= evaluateExpInt (e2, env)

-- ---------------------------------------------------------------------------
-- Tile operations (optimised)
-- ---------------------------------------------------------------------------

-- Combine side-by-side: use zipWith and cons, not [x++y] ++ rest
combineTilesRight :: TileVar -> TileVar -> TileVar
combineTilesRight (Tile xs) (Tile ys) = Tile (zipWith (++) xs ys)

-- Combine vertically: just list append (already fine)
combineTilesDown :: TileVar -> TileVar -> TileVar
combineTilesDown (Tile xs) (Tile ys) = Tile (xs ++ ys)

-- Duplicate right: use foldr1 instead of linear recursion
duplicateTileRight :: TileVar -> Int -> TileVar
duplicateTileRight t@(Tile rows) n
  | n <= 1    = t
  | otherwise = Tile (map (concat . replicate n) rows)

-- Duplicate down: replicate the rows directly
duplicateTileDown :: TileVar -> Int -> TileVar
duplicateTileDown t@(Tile rows) n
  | n <= 1    = t
  | otherwise = Tile (concat (replicate n rows))

-- Rotate 90°: transpose then reverse each row
--   Old code: hand-rolled rotateHelper with !!, map tail — O(n*m) but huge constant
--   New code: transpose is in Data.List, well-optimised
rotateTile90 :: TileVar -> TileVar
rotateTile90 (Tile xs) = Tile (map reverse (transpose xs))

-- Rotate 180°: reverse rows, then reverse each row
--   Old code: called rotateTile90 twice
rotateTile180 :: TileVar -> TileVar
rotateTile180 (Tile xs) = Tile (reverse (map reverse xs))

-- Rotate 270°: reverse each row, then transpose
--   Old code: called rotateTile90 three times!
rotateTile270 :: TileVar -> TileVar
rotateTile270 (Tile xs) = Tile (transpose (map reverse xs))

-- Square rotate: combine all 4 rotations
squareRotateTile :: TileVar -> TileVar
squareRotateTile x =
  let !r90  = rotateTile90 x
      !r180 = rotateTile180 x
      !r270 = rotateTile270 x
      !top  = combineTilesRight x r90
      !bot  = combineTilesRight r270 r180
  in combineTilesDown top bot

-- Scale: replicate each char n times, replicate each row n times
--   Old code: built tile cell-by-cell with combineTilesRight — extremely slow
scaleTile :: TileVar -> Int -> TileVar
scaleTile (Tile xs) n =
  Tile (concatMap (\row -> replicate n (concatMap (replicate n) row)) xs)

-- Blank tile: same dimensions, all '0'
createBlankTile :: TileVar -> TileVar
createBlankTile (Tile [])    = Tile []
createBlankTile (Tile (x:xs)) =
  let w = length x
      h = length (x:xs)
  in Tile (replicate h (replicate w '0'))

-- Reflections
reflectTileX :: TileVar -> TileVar
reflectTileX (Tile x) = Tile (reverse x)

reflectTileY :: TileVar -> TileVar
reflectTileY (Tile x) = Tile (map reverse x)

reflectTileXY :: TileVar -> TileVar
reflectTileXY t = reflectTileY (reflectTileX t)

-- Conjunct (AND) two tiles
conjunctTiles :: TileVar -> TileVar -> TileVar
conjunctTiles (Tile x) (Tile y) =
  Tile (zipWith (zipWith andChar) x y)
  where
    andChar '1' '1' = '1'
    andChar _   _   = '0'

-- XOR two tiles
xorTiles :: TileVar -> TileVar -> TileVar
xorTiles (Tile x) (Tile y) =
  Tile (zipWith (zipWith xorChar) x y)
  where
    xorChar '1' '0' = '1'
    xorChar '0' '1' = '1'
    xorChar _   _   = '0'

-- Negate (invert) a tile
negateTile :: TileVar -> TileVar
negateTile (Tile x) = Tile (map (map negChar) x)
  where
    negChar '0' = '1'
    negChar _   = '0'

-- Sub-tile extraction
createSubTile :: TileVar -> Int -> Int -> Int -> Int -> TileVar
createSubTile (Tile xs) xPos yPos xsize ysize =
  Tile (take ysize . drop yPos $ map (take xsize . drop xPos) xs)

-- Remove top rows
removeTop :: TileVar -> Int -> TileVar
removeTop (Tile xs) n = Tile (drop n xs)

-- Pretty print
prettyPrint :: TileVar -> String
prettyPrint (Tile xs) = intercalate "\n" xs
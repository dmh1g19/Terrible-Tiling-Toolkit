module Evaluator where

import Control.DeepSeq
import Control.Monad
import Data.List
import Grammar

-- Environment now stores TileVars directly without IO
type Environment = ([(String, TileVar)], [(String, Int)])

-- State includes expressions, environment, and accumulated output
type State = ([Exp], Environment, [String])

data TileVar = Tile [String]
  deriving (Show, Eq)

-- Evaluate expressions and return final state with output
evaluateExp :: State -> State
evaluateExp state@([], env, output) = state
evaluateExp ((ExpReturn tile1) : xs, env, output) =
  let (newTile1, env') = evaluateExpTile (tile1, env)
      newOutput = output ++ [prettyPrint newTile1]
   in (xs, env', newOutput)
evaluateExp ((ExpDoNothing : xs), env, output) = evaluateExp (xs, env, output)
evaluateExp ((MultiExpr exp1 exp2) : xs, env, output) =
  evaluateExp (exp1 : exp2 : xs, env, output)
evaluateExp ((ExpIf bool1 exp1 exp2) : xs, env, output)
  | newBool1 = evaluateExp (exp1 : xs, env, output)
  | otherwise = evaluateExp (exp2 : xs, env, output)
  where
    newBool1 = evaluateExpBool (bool1, env)
evaluateExp ((ExpWhile bool1 exp1) : xs, env, output)
  | newBool1 = evaluateExp (exp1 : exp : xs, env, output)
  | otherwise = evaluateExp (xs, env, output)
  where
    newBool1 = evaluateExpBool (bool1, env)
    exp = ExpWhile bool1 exp1
evaluateExp ((ExpPrint tile1) : xs, env, output) =
  let (newTile1, env') = evaluateExpTile (tile1, env)
      newOutput = output ++ [prettyPrint newTile1]
   in evaluateExp (xs, env', newOutput)
evaluateExp ((ExpSetTileVar name tile1) : xs, env, output) =
  let (newTile1, env') = evaluateExpTile (tile1, env)
      env'' = addTileVar name newTile1 env'
   in evaluateExp (xs, env'', output)
evaluateExp ((ExpSetIntVar name int1) : xs, env, output) =
  let newInt1 = evaluateExpInt (int1, env)
      env' = addIntVar name newInt1 env
   in evaluateExp (xs, env', output)

-- Pure version of getTileVar and helpers
getTileVar :: String -> Environment -> TileVar
getTileVar name (tiles, ints) =
  case lookup name tiles of
    Just tile -> tile
    Nothing -> error $ "Tile variable not found: " ++ name

-- Replaces the original get tile from file function
addTileVar :: String -> TileVar -> Environment -> Environment
addTileVar name tile (tiles, ints) =
  ((name, tile) : filter ((/= name) . fst) tiles, ints)

-- Evaluate Tile expressions purely
-- TODO: Implement this
evaluateExpTile :: (ExpTile, Environment) -> (TileVar, Environment)
-- Look up a tile variable
evaluateExpTile (TileVar name, env) =
  (getTileVar name env, env)
-- Combine two tiles side by side (to the right)
evaluateExpTile (TileCTR tile1 tile2, env) =
  let (newTile1, env1) = evaluateExpTile (tile1, env)
      (newTile2, env2) = evaluateExpTile (tile2, env1)
   in (combineTilesRight newTile1 newTile2, env2)
-- Combine two tiles one on top of the other (downward)
evaluateExpTile (TileCTD tile1 tile2, env) =
  let (newTile1, env1) = evaluateExpTile (tile1, env)
      (newTile2, env2) = evaluateExpTile (tile2, env1)
   in (combineTilesDown newTile1 newTile2, env2)
-- Duplicate a tile to the right a given number of times
evaluateExpTile (TileDTR tile1 int1, env) =
  let (newTile1, env1) = evaluateExpTile (tile1, env)
      newInt1 = evaluateExpInt (int1, env1)
   in (duplicateTileRight newTile1 newInt1, env1)
-- Duplicate a tile downward a given number of times
evaluateExpTile (TileDTD tile1 int1, env) =
  let (newTile1, env1) = evaluateExpTile (tile1, env)
      newInt1 = evaluateExpInt (int1, env1)
   in (duplicateTileDown newTile1 newInt1, env1)
-- Rotate a tile 90 degrees
evaluateExpTile (TileRT90 tile1, env) =
  let (newTile1, env1) = evaluateExpTile (tile1, env)
   in (rotateTile90Degrees newTile1, env1)
-- Rotate a tile 180 degrees
evaluateExpTile (TileRT180 tile1, env) =
  let (newTile1, env1) = evaluateExpTile (tile1, env)
   in (rotateTile180Degrees newTile1, env1)
-- Rotate a tile 270 degrees
evaluateExpTile (TileRT270 tile1, env) =
  let (newTile1, env1) = evaluateExpTile (tile1, env)
   in (rotateTile270Degrees newTile1, env1)
-- Square rotate a tile
evaluateExpTile (TileSRT tile1, env) =
  let (newTile1, env1) = evaluateExpTile (tile1, env)
   in (squareRotateTile newTile1, env1)
-- Scale a tile by an integer factor
evaluateExpTile (TileST tile1 int1, env) =
  let (newTile1, env1) = evaluateExpTile (tile1, env)
      newInt1 = evaluateExpInt (int1, env1)
   in (scaleTile newTile1 newInt1, env1)
-- Create a blank tile based on an existing tile
evaluateExpTile (TileCBT tile1, env) =
  let (newTile1, env1) = evaluateExpTile (tile1, env)
   in (createBlankTile newTile1, env1)
-- Reflect a tile across the X axis
evaluateExpTile (TileRTX tile1, env) =
  let (newTile1, env1) = evaluateExpTile (tile1, env)
   in (reflectTileX newTile1, env1)
-- Reflect a tile across the Y axis
evaluateExpTile (TileRTY tile1, env) =
  let (newTile1, env1) = evaluateExpTile (tile1, env)
   in (reflectTileY newTile1, env1)
-- Reflect a tile across the line Y = X
evaluateExpTile (TileRTXY tile1, env) =
  let (newTile1, env1) = evaluateExpTile (tile1, env)
   in (reflectTileXY newTile1, env1)
-- Create a sub-tile from a tile using four integer parameters
evaluateExpTile (TileSub tile1 int1 int2 int3 int4, env) =
  let (newTile1, env1) = evaluateExpTile (tile1, env)
      newInt1 = evaluateExpInt (int1, env1)
      newInt2 = evaluateExpInt (int2, env1)
      newInt3 = evaluateExpInt (int3, env1)
      newInt4 = evaluateExpInt (int4, env1)
   in (createSubTile newTile1 newInt1 newInt2 newInt3 newInt4, env1)
-- Conjunct (combine) two tiles in some specified way
evaluateExpTile (TileConjunct tile1 tile2, env) =
  let (newTile1, env1) = evaluateExpTile (tile1, env)
      (newTile2, env2) = evaluateExpTile (tile2, env1)
   in (conjunctTiles newTile1 newTile2, env2)
-- Negate (or invert) a tile's content
evaluateExpTile (TileNegate tile1, env) =
  let (newTile1, env1) = evaluateExpTile (tile1, env)
   in (negateTile newTile1, env1)
-- Remove the top n lines from a tile
evaluateExpTile (TileRemoveTop tile1 int1, env) =
  let (newTile1, env1) = evaluateExpTile (tile1, env)
      newInt1 = evaluateExpInt (int1, env1)
   in (removeTop newTile1 newInt1, env1)

-- Functions for adding/getting int variables
addIntVar :: String -> Int -> Environment -> Environment
addIntVar name int (tiles, ints)
  | variableNameExists name ints = (tiles, (replaceIntVar name int ints))
  | otherwise = (tiles, (name, int) : ints)

getIntVar :: String -> Environment -> Int
getIntVar name (tiles, ints)
  | variableNameExists name ints = getIntVariable name ints
  | otherwise = error ("Error: Variable name for integers: " ++ name ++ " does not exist")

getIntVariable :: String -> [(String, Int)] -> Int
getIntVariable name ((var, int) : xs)
  | name == var = int
  | otherwise = getIntVariable name xs

replaceIntVar :: String -> Int -> [(String, Int)] -> [(String, Int)]
replaceIntVar name int ((var, intvar) : xs)
  | name == var = (name, int) : xs
  | otherwise = (var, intvar) : replaceIntVar name int xs

-- Evaluates an integer expression to an integer
evaluateExpInt :: (ExpInt, Environment) -> Int
evaluateExpInt ((IntVal int1), env) = int1
evaluateExpInt ((IntVar name), env) = getIntVar name env
evaluateExpInt ((IntNegate int1), env) = -evaluateExpInt (int1, env)
evaluateExpInt ((IntPlus int1 int2), env) = (newInt1) + (newInt2)
  where
    newInt1 = evaluateExpInt (int1, env)
    newInt2 = evaluateExpInt (int2, env)
evaluateExpInt ((IntMinus int1 int2), env) = (newInt1) - (newInt2)
  where
    newInt1 = evaluateExpInt (int1, env)
    newInt2 = evaluateExpInt (int2, env)
evaluateExpInt ((IntTimes int1 int2), env) = (newInt1) * (newInt2)
  where
    newInt1 = evaluateExpInt (int1, env)
    newInt2 = evaluateExpInt (int2, env)
evaluateExpInt ((IntDivide int1 int2), env) = (newInt1) `div` (newInt2)
  where
    newInt1 = evaluateExpInt (int1, env)
    newInt2 = evaluateExpInt (int2, env)
evaluateExpInt ((IntExponential int1 int2), env) = (newInt1) ^ (newInt2)
  where
    newInt1 = evaluateExpInt (int1, env)
    newInt2 = evaluateExpInt (int2, env)

-- Evaluates a boolean expressions to a boolean
evaluateExpBool :: (ExpBool, Environment) -> Bool
evaluateExpBool (BoolTrue, env) = True
evaluateExpBool (BoolFalse, env) = False
evaluateExpBool ((BoolAnd bool1 bool2), env) = (newBool1) && (newBool2)
  where
    newBool1 = evaluateExpBool (bool1, env)
    newBool2 = evaluateExpBool (bool2, env)
evaluateExpBool ((BoolOr bool1 bool2), env) = (newBool1) && (newBool2)
  where
    newBool1 = evaluateExpBool (bool1, env)
    newBool2 = evaluateExpBool (bool2, env)
evaluateExpBool ((BoolNot bool1), env) = not (newBool1)
  where
    newBool1 = evaluateExpBool (bool1, env)
evaluateExpBool ((BoolLessThan exp1 exp2), env) = (newInt1) < (newInt2)
  where
    newInt1 = evaluateExpInt (exp1, env)
    newInt2 = evaluateExpInt (exp2, env)
evaluateExpBool ((BoolLessEqualThan exp1 exp2), env) = (newInt1) <= (newInt2)
  where
    newInt1 = evaluateExpInt (exp1, env)
    newInt2 = evaluateExpInt (exp2, env)
evaluateExpBool ((BoolMoreThan exp1 exp2), env) = (newInt1) > (newInt2)
  where
    newInt1 = evaluateExpInt (exp1, env)
    newInt2 = evaluateExpInt (exp2, env)
evaluateExpBool ((BoolMoreEqualThan exp1 exp2), env) = (newInt1) >= (newInt2)
  where
    newInt1 = evaluateExpInt (exp1, env)
    newInt2 = evaluateExpInt (exp2, env)
evaluateExpBool ((BoolEqual exp1 exp2), env) = (newInt1) == (newInt2)
  where
    newInt1 = evaluateExpInt (exp1, env)
    newInt2 = evaluateExpInt (exp2, env)
evaluateExpBool ((BoolNotEqual exp1 exp2), env) = (newInt1) /= (newInt2)
  where
    newInt1 = evaluateExpInt (exp1, env)
    newInt2 = evaluateExpInt (exp2, env)

-- Converts from Bool to ExpBool
convBoolToExpBool :: Bool -> ExpBool
convBoolToExpBool True = BoolTrue
convBoolToExpBool False = BoolFalse

-- Converts from Int to ExpInt
convIntToExpInt :: Int -> ExpInt
convIntToExpInt val = IntVal val

-- Used to Combine two Tiles to the right
combineTilesRight :: TileVar -> TileVar -> TileVar
combineTilesRight (Tile xs) (Tile ys) = Tile (combineLineArrays xs ys)

combineLineArrays :: [String] -> [String] -> [String]
combineLineArrays [] [] = []
combineLineArrays _ [] = error "Not same tile size"
combineLineArrays [] _ = error "Not same tile size"
combineLineArrays (x : xs) (y : ys) = [x ++ y] ++ combineLineArrays xs ys

-- Used to duplicate a tile right for specified amount
duplicateTileRight :: TileVar -> Int -> TileVar
duplicateTileRight (tile) amount
  | amount == 1 = tile
  | otherwise = (combineTilesRight (tile) (duplicateTileRight tile (amount - 1)))

-- Used to duplicate a tile down for a specified amount
duplicateTileDown :: TileVar -> Int -> TileVar
duplicateTileDown (tile) amount
  | amount == 1 = tile
  | otherwise = (combineTilesDown (tile) (duplicateTileDown tile (amount - 1)))

-- Used to combine two tiles above/below each other
combineTilesDown :: TileVar -> TileVar -> TileVar
combineTilesDown (Tile xs) (Tile ys) = Tile (xs ++ ys)

-- Used to rotate a tile
rotateTile90Degrees (Tile xs) = Tile (rotateHelper (xs))

rotateTile180Degrees (tile) = rotateTile90Degrees $ rotateTile90Degrees tile

rotateTile270Degrees (tile) = rotateTile90Degrees $ rotateTile90Degrees $ rotateTile90Degrees tile

rotateHelper :: [String] -> [String]
rotateHelper (xs)
  | length (xs !! 0) > 1 = getFirst xs : rotateHelper (map tail xs)
  | otherwise = [getFirst xs]

getFirst :: [String] -> String
getFirst [] = []
getFirst (x : xs) = getFirst xs ++ [head x]

-- Combines all 4 rotated tiles into one big tile
squareRotateTile (x) = combineTilesDown (combineTilesRight x (rotateTile90Degrees x)) (combineTilesRight (rotateTile270Degrees x) (rotateTile180Degrees x))

-- Used to Scale a Tile
scaleTile :: TileVar -> Int -> TileVar
scaleTile (Tile xs) amount = (scaleTileRow xs amount)

scaleTileRow :: [String] -> Int -> TileVar
scaleTileRow [x] amount = (scaleTileLine (x) amount)
scaleTileRow (x : xs) amount = combineTilesDown (scaleTileLine (x) amount) (scaleTileRow xs amount)

scaleTileLine :: String -> Int -> TileVar
scaleTileLine [x] amount = Tile (scaleTileSingular x amount)
scaleTileLine (x : xs) amount = combineTilesRight (Tile (scaleTileSingular x amount)) (scaleTileLine xs amount)

scaleTileSingular :: Char -> Int -> [String]
scaleTileSingular x amount = replicate amount $ (concat $ replicate amount [x])

-- Used to create a blank tile replica of the same size of the input tile
createBlankTile :: TileVar -> TileVar
createBlankTile (Tile [x]) = Tile [(concat $ replicate (length x) "0")]
createBlankTile (Tile (x : xs)) = Tile (replicate (length (x : xs)) $ (concat $ replicate (length x) "0"))

-- Used to reflect a tile by x axis
reflectTileX :: TileVar -> TileVar
reflectTileX (Tile x) = Tile (reverse x)

-- Used to reflect a tile by y axis
reflectTileY :: TileVar -> TileVar
reflectTileY (Tile x) = Tile (map reverse x)

-- Used to reflect a tile by y=x
reflectTileXY :: TileVar -> TileVar
reflectTileXY (tile) = reflectTileY $ reflectTileX tile

-- Used to conjunct two tiles
conjunctTiles :: TileVar -> TileVar -> TileVar
conjunctTiles (Tile x) (Tile y) = Tile (map zipTiles (zip x y))
  where
    zipTiles (x, y) = zipWith (\x y -> if all (== '1') [x, y] then '1' else '0') x y

-- Used to negate a tile
negateTile :: TileVar -> TileVar
negateTile (Tile x) = Tile (map negateRow x)
  where
    negateRow = map (\x -> if x == '0' then '1' else '0')

-- Used to create sub tiles from starting positions and lengths
createSubTile :: TileVar -> Int -> Int -> Int -> Int -> TileVar
createSubTile (Tile xs) (xPos) (yPos) xsize ysize = Tile (splitList yPos ysize $ map (splitList xPos xsize) xs)

splitList :: Int -> Int -> [a] -> [a]
splitList startPos length xs = take (length) (drop (startPos) xs)

-- Used to remove a top row of tiles
removeTop :: TileVar -> Int -> TileVar
removeTop (Tile xs) val = Tile (drop val xs)

variableNameExists :: String -> [(String, a)] -> Bool
variableNameExists name [] = False
variableNameExists name ((var, tile) : xs)
  | name == var = True
  | otherwise = variableNameExists name xs

-- Remove IO from prettyPrint
prettyPrint :: TileVar -> String
prettyPrint (Tile xs) = intercalate "\n" xs

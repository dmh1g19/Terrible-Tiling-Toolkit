{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
module Main where

import Miso
import Miso.String (MisoString, ms)
import qualified Data.JSString as JS
import Control.Exception (try, SomeException, evaluate)
import Control.DeepSeq (force)
import Tsl (runTslInBrowser)
import qualified TileView
import Data.List (find)
import Data.Monoid (mconcat)

-- | Split a MisoString on newline characters.
splitNewlines :: MisoString -> [MisoString]
splitNewlines s = map ms (lines (JS.unpack s))

-- | Data type for pre-loaded examples.
data Example = Example
  { exampleName  :: MisoString
  , exampleDSL   :: MisoString
  , exampleTiles :: [(MisoString, MisoString)]
  } deriving (Show, Eq)

example1 :: Example
example1 = Example
  { exampleName  = "Checker Board"
  , exampleDSL   = "getTileFile tile1 inp1;\ngetTileFile tile2 inp2;\n\ntile1 = combineTilesRight(inp1,inp2);\ntile2 = duplicateTileRight(tile1,32);\ntile3 = combineTilesDown(tile2, rotateTile180Degrees(tile2));\ntile4 = duplicateTileDown(tile3,32);\n\nreturn tile4;"
  , exampleTiles = [("tile1", "0"), ("tile2", "1")]
  }

example2 :: Example
example2 = Example
  { exampleName  = "Rotating and Scaling"
  , exampleDSL   = "getTileFile tile1 inp1;\n\ntile1 = squareRotateTile(inp1);\ntile2 = combineTilesRight(tile1,tile1);\ntile3 = combineTilesRight(tile2, (combineTilesDown(inp1,rotateTile270Degrees(inp1))));\ntile4 = combineTilesDown((tile3),(combineTilesRight((combineTilesDown(tile1,(combineTilesRight(inp1,(rotateTile90Degrees(inp1)))))),(scaleTile(inp1,3)))));\ntile5 = squareRotateTile(tile4);\n\nreturn tile5;"
  , exampleTiles = [("tile1", "0001 0011 0111 1111")]
  }

example3 :: Example
example3 = Example
  { exampleName  = "Reflections and blanks"
  , exampleDSL   = "getTileFile tile1 inp1;\n\ntile1 = combineTilesRight((combineTilesRight(inp1,(reflectTileY(inp1)))), (createBlankTile(inp1)));\ntile2 = combineTilesRight((combineTilesRight((createBlankTile(inp1)),(reflectTileX(inp1)))), (reflectTileXY(inp1)));\nbaseTile = combineTilesDown(tile1,tile2);\ntile3 = combineTilesRight(baseTile,(reflectTileX(baseTile)));\ntile4 = combineTilesDown(tile3, (tile3));\ntile5 = duplicateTileRight(tile4, 10);\ntile6 = duplicateTileDown(tile5, 15);\nreturn tile6;"
  , exampleTiles = [("tile1", "000 000 011")]
  }

example4 :: Example
example4 = Example
  { exampleName  = "Boolean Ops and Predicates"
  , exampleDSL   = "getTileFile tile1 inp1;\ngetTileFile tile2 inp2;\ngetTileFile tile3 inp3;\n\n--Create a blank tile of length 50\nblankTile = createBlankTile(inp1);\nconjunct1Neg3 = conjunctTiles(inp1, negateTile(inp3));\nconjunct2Neg3 = conjunctTiles(inp2, negateTile(inp3));\n\nrow = 0;\ncolumn = 0;\n\nrowTile = duplicateTileRight(blankTile, 50);\ncolumnTile = createBlankTile(inp1);\n\nwhile (row < 50){\n\n\tif(((row + column) < 50) && (column < 25)){\n\t\tcolumnTile = conjunctTiles(inp1, negateTile(inp3));\n\t} else{\n\t\tif((row <= column) && (column >= 25)){\n\t\t\tcolumnTile = conjunctTiles(inp2, negateTile(inp3));\n\t\t} else{\n\t\t\tcolumnTile = createBlankTile(inp1);\n\t\t};\n\t};\n\tcolumn = column+1;\n\n\twhile(column < 50){\n\t\tif((row + column < 50) && (column < 25)){\n\t\t\tcolumnTile = combineTilesRight(columnTile, conjunctTiles(inp1, negateTile(inp3)));\n\t\t} else{\n\t\t\tif((row <= column) && (column >= 25)){\n\t\t\t\tcolumnTile = combineTilesRight(columnTile, conjunctTiles(inp2, negateTile(inp3)));\n\t\t\t} else{\n\t\t\t\tcolumnTile = combineTilesRight(columnTile, blankTile);\n\t\t\t};\n\t\t};\n\n\t\tcolumn = column+1;\n\t};\n\trowTile = combineTilesDown(rowTile, columnTile);\n\trow = row + 1;\n\tcolumn = 0;\n};\n\nfinalTile = removeTop(rowTile, 2);\n\nreturn finalTile;"
  , exampleTiles = [("tile1", "11 00"), ("tile2", "00 11"), ("tile3", "01 10")]
  }

example5 :: Example
example5 = Example
  { exampleName  = "Subtiles"
  , exampleDSL   = "getTileFile tile1 inp1;\n\ntile1 = duplicateTileRight( createSubTile(inp1,0,0,6,6), 3);\ntile2 = duplicateTileRight( createSubTile(inp1,2,2,6,6), 3);\ntile3 = duplicateTileRight( createSubTile(inp1,4,4,6,6), 3);\n\ntile4 = combineTilesDown(combineTilesDown(tile1,tile2),tile3);\nreturn tile4;"
  , exampleTiles = [("tile1", "1100000011 1100000011 0011001111 0011001111 0000110011 0000110011 0011001111 0011001111 1111111111 1111111111")]
  }

example6 :: Example
example6 = Example
  { exampleName  = "Three way alternation with chamfered corners"
  , exampleDSL   = "getTileFile tile1 inp1;\ngetTileFile tile2 inp2;\ngetTileFile tile3 inp3;\n\nblankTile = createBlankTile(inp1);\n\n-- First tile\ni = 0;\nfirstTile = combineTilesRight(blankTile, blankTile);\nwhile (i < 19) {\n\tfirstTile = combineTilesRight(firstTile, inp1);\n\tfirstTile = combineTilesRight(firstTile, inp2);\n\tfirstTile = combineTilesRight(firstTile, inp3);\n\ti = i + 1;\t\n};\nfirstTile = combineTilesRight(firstTile, inp1);\n\n-- Second Tile\ni = 0;\nsecondTile = createBlankTile(inp1);\nwhile (i < 19) {\n\tsecondTile = combineTilesRight(secondTile, inp2);\n\tsecondTile = combineTilesRight(secondTile, inp3);\n\tsecondTile = combineTilesRight(secondTile, inp1);\n\ti = i + 1;\t\n};\nsecondTile = combineTilesRight(secondTile, inp2);\nsecondTile = combineTilesRight(secondTile, inp3);\n\nmainTile = combineTilesDown(firstTile, secondTile);\n\nj = 1;\ni=0;\nwhile (i < 56) {\n\tif (j==1) {\n\t\ttempTile = combineTilesRight(inp3, inp1);\n\t\ttempTile = combineTilesRight(tempTile, inp2);\n\t\tk=0;\n\t\twhile (k<19) {\n\t\t\ttempTile = combineTilesRight(tempTile, inp3);\t\n\t\t\ttempTile = combineTilesRight(tempTile, inp1);\t\n\t\t\ttempTile = combineTilesRight(tempTile, inp2);\t\n\t\t\tk=k+1;\n\t\t};\n\t\tmainTile = combineTilesDown(mainTile, tempTile); \n\t} else { doNothing;};\n\n\tif (j==2) {\n\t\ttempTile = combineTilesRight(inp2, inp3);\n\t\ttempTile = combineTilesRight(tempTile, inp1);\n\t\tk=0;\n\t\twhile (k<19) {\n\t\t\ttempTile = combineTilesRight(tempTile, inp2);\t\n\t\t\ttempTile = combineTilesRight(tempTile, inp3);\t\n\t\t\ttempTile = combineTilesRight(tempTile, inp1);\t\n\t\t\tk=k+1;\n\t\t};\n\t\tmainTile = combineTilesDown(mainTile, tempTile); \n\t} else {doNothing;};\n\n\tif (j==3) {\n\t\ttempTile = combineTilesRight(inp1, inp2);\n\t\ttempTile = combineTilesRight(tempTile, inp3);\n\t\tk=0;\n\t\twhile (k<19) {\n\t\t\ttempTile = combineTilesRight(tempTile, inp1);\t\n\t\t\ttempTile = combineTilesRight(tempTile, inp2);\t\n\t\t\ttempTile = combineTilesRight(tempTile, inp3);\n\t\t\tk=k+1;\t\n\t\t};\n\t\tmainTile = combineTilesDown(mainTile, tempTile); \n\t\tj=0;\n\t}else {doNothing;};\n\tj=j+1;\n\ti=i+1;\n};\n\n-- Second last tile\nsecondLastTile = combineTilesRight(inp1, inp2);\nsecondLastTile = combineTilesRight(secondLastTile, inp3);\nk=0;\nwhile (k<19) {\n\tsecondLastTile = combineTilesRight(secondLastTile, inp1);\t\n\tsecondLastTile = combineTilesRight(secondLastTile, inp2);\n\tif(k!=18){\n\t\tsecondLastTile = combineTilesRight(secondLastTile, inp3);\n\t} else {doNothing;};\t\n\t\n\tk=k+1;\n};\nsecondLastTile = combineTilesRight(secondLastTile, blankTile);\n\n-- Last tile\nlastTile = combineTilesRight(inp3, inp1);\nk=0;\nwhile (k<19) {\n\tlastTile = combineTilesRight(lastTile, inp2);\n\tlastTile = combineTilesRight(lastTile, inp3);\n\tif(k!=18){\n\t\tlastTile = combineTilesRight(lastTile, inp1);\t\n\t} else {doNothing;};\n\tk=k+1;\n};\nlastTile = combineTilesRight(lastTile, blankTile);\nlastTile = combineTilesRight(lastTile, blankTile);\n\nmainTile = combineTilesDown(mainTile, secondLastTile);\nmainTile = combineTilesDown(mainTile, lastTile);\n\nreturn mainTile;"
  , exampleTiles = [("tile1", "111 111 111"), ("tile2", "010 111 010"), ("tile3", "101 010 101")]
  }

example7 :: Example
example7 = Example
  { exampleName  = "Repeated scaling"
  , exampleDSL   = "S = 35;\n\ngetTileFile tile1 inp1;\n\nsubTilep1 = combineTilesRight(inp1, reflectTileY(inp1));\nsubTilep2 = combineTilesRight(reflectTileX(inp1), reflectTileXY(inp1));\nsubTile = combineTilesDown(subTilep1, subTilep2);\n\nblankRow = duplicateTileRight(createBlankTile(subTile), S);\noutputTile = scaleTile(subTile, S);\n\nx = S - 2;\nwhile(x > 0){\n\ttopRow = duplicateTileDown(blankRow, (S - x));\n\tleftPart = duplicateTileDown(duplicateTileRight(createBlankTile(subTile), (S - x)), x);\n\trightPart = scaleTile(subTile, x);\n\n\tcombinedLR = combineTilesRight(leftPart, rightPart);\n\tbigTile = combineTilesDown(topRow, combinedLR);\n\n\toutputTile = negateTile(\n\t\t conjunctTiles(\n\t\t negateTile(conjunctTiles(outputTile, negateTile(bigTile))),\n\t\t negateTile(conjunctTiles(negateTile(outputTile), bigTile))\n\t\t )\n\t );\n\tx = x - 2;\n};\n\nreturn outputTile;"
  , exampleTiles = [("tile1", "01 11")]
  }

example8 :: Example
example8 = Example
  { exampleName  = "Awkwardly composition"
  , exampleDSL   = "getTileFile tile1 blue;\ngetTileFile tile2 green;\n\ngreen2Up = scaleTile(green,2);\ngreenDown = reflectTileX(green);\ngreen2Down = reflectTileX(green2Up);\nblueDown = reflectTileX (blue);\n\n\ntile1 = combineTilesRight( combineTilesRight(green2Down,green2Down) , combineTilesDown(greenDown,blue));\ntile2 = combineTilesDown( combineTilesDown(greenDown,blue), blue );\ntile3 = combineTilesDown(green2Down, combineTilesRight(blue,green));\ntile4 = combineTilesDown( combineTilesRight(blue,blue), green2Up);\ntile5 = combineTilesRight( combineTilesRight(tile2,tile3), tile4);\nsubTile1 = combineTilesDown(tile1,tile5);\n\n\ntile6 = combineTilesRight(combineTilesDown(greenDown,blue), combineTilesRight(green2Down,green2Down));\ntile7 = combineTilesDown(green2Down, combineTilesRight(green,blue));\ntile8 = combineTilesRight(tile4, combineTilesRight(tile7,tile2));\nsubTile2 = combineTilesDown(tile6,tile8);\n\n\ntile9 = combineTilesRight( combineTilesRight(blueDown,blueDown), greenDown);\ntile10 = combineTilesRight( combineTilesDown (blueDown,green), green2Up);\ntile11 = combineTilesRight( combineTilesDown (greenDown,blue), green2Down);\nsubTile3 = combineTilesDown( combineTilesDown (tile9,tile10), tile11);\n\n\ntile12 = combineTilesRight ( combineTilesRight(greenDown,blueDown), blueDown);\ntile13 = combineTilesRight (green2Up, combineTilesDown(blueDown,green));\ntile14 = combineTilesRight (green2Down, combineTilesDown(greenDown,blue));\nsubTile4 = combineTilesDown (combineTilesDown(tile12,tile13), tile14);\n\n\ntileSubRow1 = combineTilesRight(subTile3, subTile1);\ntileSubRow2 = combineTilesRight(subTile2, subTile4);\ntileSubRow3 = combineTilesRight(tileSubRow1, tileSubRow2);\ntileSubRowFinal = combineTilesRight(duplicateTileRight(tileSubRow3,2), combineTilesRight(subTile3,subTile1));\n\n\n\nfinalTiles1 = combineTilesDown(tileSubRowFinal, reflectTileX(tileSubRowFinal));\nfinalTile = duplicateTileDown(finalTiles1,4);\n\nreturn finalTile;"
  , exampleTiles = [("tile1", "010 001 110"), ("tile2", "010 101 101")]
  }

example9 :: Example
example9 = Example
  { exampleName  = "Diamond and gradients"
  , exampleDSL   = "getTileFile tile1 inp1;\n\nrow = 0;\ncolumn = 0;\n\ncolumnTile = createBlankTile(inp1);\n\nif(2*column < row){\n\tcolumnTile = conjunctTiles(inp1,inp1);\n} else{\n\tcolumnTile = createBlankTile(inp1);\n};\ncolumn = column+1;\n\nwhile(column < 101){\n\tif((2*column) < row){\n\t\tcolumnTile = combineTilesRight(columnTile, inp1);\n\t} else{\n\t\tcolumnTile = combineTilesRight(columnTile, createBlankTile(inp1));\n    };\n    column = column+1;\n};\n\n\nrowTile = conjunctTiles(columnTile, columnTile);\nrow = row + 1;\ncolumn = 0;\n\nwhile (row < 101){\n\n\tif(2*column < row){\n\t\tcolumnTile = conjunctTiles(inp1,inp1);\n\t} else{\n\t\tcolumnTile = createBlankTile(inp1);\n\t};\n\tcolumn = column+1;\n\n\twhile(column < 101){\n\t\tif(2*column < row){\n\t\t    columnTile = combineTilesRight(columnTile, inp1);\n\t    } else{\n\t\t    columnTile = combineTilesRight(columnTile, createBlankTile(inp1));\n        };\n        column = column+1;\n\t};\n\trowTile = combineTilesDown(rowTile, columnTile);\n\trow = row + 1;\n\tcolumn = 0;\n};\n\ntileReflectY = reflectTileY(rowTile);\ntile270 = rotateTile270Degrees(rowTile);\ntile90Y = reflectTileY(rotateTile90Degrees(rowTile));\n\ntileP1 = negateTile(conjunctTiles(negateTile ( conjunctTiles(rowTile, negateTile(tile270)) ), negateTile( conjunctTiles(negateTile(rowTile),tile270) )));\ntileP2 = negateTile(conjunctTiles(negateTile ( conjunctTiles(tileReflectY, negateTile(tile90Y)) ), negateTile( conjunctTiles(negateTile(tileReflectY),tile90Y) )));\ntileP3 = negateTile(conjunctTiles(negateTile ( conjunctTiles(tileP1, negateTile(tileP2)) ), negateTile( conjunctTiles(negateTile(tileP1),tileP2) )));\nreturn tileP3;"
  , exampleTiles = [("tile1", "010 101 010"), ("tile2", "00 11"), ("tile3", "01 10")]
  }

example10 :: Example
example10 = Example
  { exampleName  = "Partial replacement"
  , exampleDSL   = "getTileFile tile1 inp1;\ngetTileFile tile2 inp2;\n\ntile1 = createSubTile(inp1,0,0,999999,12);\ntile2 = createSubTile(inp1,0,12,12,5);\ntile3 = createSubTile(inp1,17,12,999999,5);\ntile4 = createSubTile(inp1,0,17,999999,999999);\ntile5 = combineTilesRight(combineTilesRight(tile2,inp2),tile3);\ntile6 = combineTilesDown(combineTilesDown(tile1,tile5),tile4);\n\nreturn tile6;"
  , exampleTiles = [("tile1", "00000000000000000000 00000011111111000000 00000111111111100000 00001111111111110000 00011111111111111000 00111001111110011100 00111001111110011100 00111111111111111100 01111111111111111110 01111111111111111110 01111111111111111110 01111111111111111110 00111111111111111100 00111001111111001100 00111100000000011100 00011111111111111000 00001111111111110000 00000111111111100000 00000011111111000000 00000000000000000000"), ("tile2", "11111 10101 11011 10101 11111")]
  }

examples :: [Example]
examples = [example1, example2, example3, example4, example5, example6, example7, example8, example9, example10]

-- | Our model includes the DSL code, evaluation output, tile inputs,
--   and temporary inputs for a new tile.
data Model = Model
  { dslInput     :: MisoString
  , output       :: [MisoString]
  , tileInputs   :: [(MisoString, MisoString)]
  , newTileName  :: MisoString
  , newTileShape :: MisoString
  } deriving (Show, Eq)

-- | Actions include updating inputs, adding/removing tiles, evaluating DSL,
--   processing results, and selecting a pre-loaded example.
data Action
  = DSLInputChanged MisoString
  | EvaluateDSL
  | DSLResult (Either MisoString [MisoString])
  | NewTileNameChanged MisoString
  | NewTileShapeChanged MisoString
  | AddTile
  | RemoveTile Int
  | SelectExample MisoString
  | NoOp
  deriving (Show, Eq)

main :: IO ()
main = startApp App
  { initialAction = NoOp
  , model = Model
      { dslInput     = ""
      , output       = []
      , tileInputs   = []
      , newTileName  = ""
      , newTileShape = ""
      }
  , update = updateModel
  , view = viewModel
  , events = defaultEvents
  , subs = []
  , mountPoint = Nothing
  }

updateModel :: Action -> Model -> Effect Action Model
updateModel (DSLInputChanged newInput) model = noEff model { dslInput = newInput }
updateModel (NewTileNameChanged newName) model = noEff model { newTileName = newName }
updateModel (NewTileShapeChanged newShape) model = noEff model { newTileShape = newShape }
updateModel AddTile model@Model { newTileName = name, newTileShape = shape, tileInputs = tiles } =
  noEff model { tileInputs = tiles ++ [(name, shape)]
              , newTileName  = ""
              , newTileShape = ""
              }
updateModel (RemoveTile idx) model@Model { tileInputs = tiles } =
  let newTiles = take idx tiles ++ drop (idx + 1) tiles
  in noEff model { tileInputs = newTiles }
updateModel (SelectExample exName) model =
  case find (\ex -> exampleName ex == exName) examples of
    Just ex -> noEff model { dslInput = exampleDSL ex, tileInputs = exampleTiles ex }
    Nothing -> noEff model
updateModel EvaluateDSL model@Model { dslInput = dsl, tileInputs = tiles } = model <# do
  let code = JS.unpack dsl
      tileData = map (\(n, s) -> (JS.unpack n, JS.unpack s)) tiles
  result <- try (evaluate (force (runTslInBrowser code tileData))) :: IO (Either SomeException [String])
  case result of
    Left err   -> pure $ DSLResult (Left (ms (show err)))
    Right outs -> pure $ DSLResult (Right (map ms outs))
updateModel (DSLResult res) model =
  case res of
    Left errMsg -> noEff model { output = [ "Error: " <> errMsg ] }
    Right outs  -> noEff model { output = outs }
updateModel NoOp model = noEff model

viewModel :: Model -> View Action
viewModel Model {..} = div_ [ style_ containerStyle ]
  [ div_ [ style_ leftColumnStyle ]
      [ h1_ [ style_ headerStyle ] [ text "Examples" ]
      , select_ [ onChange (\val -> SelectExample val), style_ dropdownStyle ]
          (defaultOption : map optionView examples)
      , h1_ [ style_ headerStyle ] [ text "DSL Editor" ]
      , textarea_
          [ onInput DSLInputChanged
          , style_ (mconcat [ "width" =: "100%", "height" =: "200px", "margin-bottom" =: "10px", "padding" =: "10px", "font-size" =: "14px" ])
          ]
          [ text dslInput ]
      , button_ [ onClick EvaluateDSL, style_ buttonStyle ] [ text "Evaluate DSL" ]
      , h2_ [ style_ headerStyle ] [ text "Tile Inputs:" ]
      , div_ [] (map (uncurry viewTile) (zip [0..] tileInputs))
      , h2_ [ style_ headerStyle ] [ text "Add New Tile:" ]
      , div_ [ style_ (mconcat [ "margin-bottom" =: "20px" ]) ]
          [ input_ [ placeholder_ "Tile Name", value_ newTileName, onInput NewTileNameChanged, style_ inputStyle ]
          , input_ [ placeholder_ "Tile Shape", value_ newTileShape, onInput NewTileShapeChanged, style_ inputStyle ]
          , button_ [ onClick AddTile, style_ buttonStyle ] [ text "Add Tile" ]
          ]
      ]
  , div_ [ style_ rightColumnStyle ]
      [ h1_ [ style_ headerStyle ] [ text "Tile Output:" ]
      , TileView.viewTileSVG (concatMap splitNewlines output)
      ]
  ]
  where
    containerStyle = mconcat
      [ "display" =: "flex"
      , "flex-direction" =: "row"
      , "font-family" =: "Arial, sans-serif"
      ]
    leftColumnStyle = mconcat
      [ "width" =: "50%"
      , "padding" =: "20px"
      , "background-color" =: "#f7f7f7"
      , "box-shadow" =: "2px 0px 5px rgba(0,0,0,0.1)"
      ]
    rightColumnStyle = mconcat
      [ "width" =: "50%"
      , "padding" =: "20px"
      ]
    headerStyle = mconcat [ "color" =: "#333", "margin-bottom" =: "10px" ]
    inputStyle = mconcat
      [ "padding" =: "8px"
      , "margin-right" =: "10px"
      , "border" =: "1px solid #ccc"
      , "border-radius" =: "4px"
      ]
    buttonStyle = mconcat
      [ "padding" =: "10px 15px"
      , "background-color" =: "#3498db"
      , "color" =: "white"
      , "border" =: "none"
      , "border-radius" =: "4px"
      , "cursor" =: "pointer"
      , "margin-right" =: "10px"
      ]
    dropdownStyle = mconcat
      [ "width" =: "100%"
      , "padding" =: "8px"
      , "margin-bottom" =: "20px"
      , "border" =: "1px solid #ccc"
      , "border-radius" =: "4px"
      ]
    defaultOption = option_ [ value_ "" ] [ text "Select an example" ]
    optionView :: Example -> View Action
    optionView ex = option_ [ value_ (exampleName ex) ] [ text (exampleName ex) ]
    viewTile :: Int -> (MisoString, MisoString) -> View Action
    viewTile idx (name, shape) =
      div_ [ style_ (mconcat [ "margin-bottom" =: "10px"
                              , "padding" =: "5px"
                              , "background-color" =: "#fff"
                              , "border" =: "1px solid #ddd"
                              , "border-radius" =: "4px"
                              , "display" =: "flex"
                              , "justify-content" =: "space-between"
                              , "align-items" =: "center"
                              ])
           ]
        [ span_ [ style_ (mconcat [ "margin-right" =: "10px" ]) ]
            [ text ("Name: " <> name <> ", Shape: " <> shape) ]
        , button_ [ onClick (RemoveTile idx)
                  , style_ (mconcat [ "padding" =: "5px 10px"
                                    , "background-color" =: "#e74c3c"
                                    , "color" =: "white"
                                    , "border" =: "none"
                                    , "border-radius" =: "4px"
                                    , "cursor" =: "pointer"
                                    ])
                  ]
                  [ text "Remove" ]
        ]

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
import Examples
import Data.List (find)
import Data.Monoid (mconcat)

-- | Split a MisoString on newline characters.
splitNewlines :: MisoString -> [MisoString]
splitNewlines s = map ms (lines (JS.unpack s))

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
          , style_ (mconcat [ "width" =: "100%"
                           , "height" =: "200px"
                           , "margin-bottom" =: "10px"
                           , "padding" =: "10px"
                           , "font-size" =: "14px"
                           ])
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
      , if not (null output) && ("Error:" `JS.isPrefixOf` head output)
           then div_ [ style_ errorStyle ] [ text (head output) ]
           else TileView.viewTileSVG (concatMap splitNewlines output)
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
    errorStyle = mconcat [ "color" =: "red"
                         , "font-weight" =: "bold"
                         , "padding" =: "10px"
                         ]

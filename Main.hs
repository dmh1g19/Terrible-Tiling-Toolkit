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
import Data.Aeson (withObject, (.:))

-- | Split a MisoString on newline characters.
splitNewlines :: MisoString -> [MisoString]
splitNewlines s = map ms (lines (JS.unpack s))

examples :: [Example]
examples = [example1, example2, example3, example4, example5,
            example6, example7, example8, example9, example10]

-- ---------------------------------------------------------------------------
-- Model
-- ---------------------------------------------------------------------------

data Model = Model
  { dslInput     :: MisoString
  , output       :: [MisoString]
  , tileInputs   :: [(MisoString, MisoString)]
  , newTileName  :: MisoString
  , newTileShape :: MisoString
  -- Viewport (CSS-transform based — values are in screen pixels)
  , vpZoom       :: Float   -- 1.0 = content fits container
  , vpPanX       :: Float   -- horizontal shift in px
  , vpPanY       :: Float   -- vertical shift in px
  , isDragging   :: Bool
  , lastMouseX   :: Float
  , lastMouseY   :: Float
  } deriving (Show, Eq)

-- ---------------------------------------------------------------------------
-- Action
-- ---------------------------------------------------------------------------

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
  -- Viewport
  | StartDrag Float Float
  | DragMove  Float Float
  | StopDrag
  | WheelZoom Float Float Float   -- deltaY, offsetX, offsetY
  | ResetViewport
  deriving (Show, Eq)

-- ---------------------------------------------------------------------------
-- Event decoders
-- ---------------------------------------------------------------------------

mousePositionDecoder :: Decoder (Float, Float)
mousePositionDecoder = Decoder
  { decodeAt = DecodeTarget mempty
  , decoder  = withObject "MouseEvent" $ \o -> do
      x <- o .: "clientX"
      y <- o .: "clientY"
      return (x, y)
  }

-- | Decodes deltaY and the mouse offset within the target element,
--   so we can zoom towards the cursor position.
wheelDecoder :: Decoder (Float, Float, Float)
wheelDecoder = Decoder
  { decodeAt = DecodeTarget mempty
  , decoder  = withObject "WheelEvent" $ \o -> do
      dy <- o .: "deltaY"
      ox <- o .: "offsetX"
      oy <- o .: "offsetY"
      return (dy, ox, oy)
  }

-- ---------------------------------------------------------------------------
-- Zoom constants
-- ---------------------------------------------------------------------------

zoomMin, zoomMax, zoomFactor :: Float
zoomMin    = 0.2
zoomMax    = 30.0
zoomFactor = 1.1

clampZoom :: Float -> Float
clampZoom z = max zoomMin (min zoomMax z)

-- ---------------------------------------------------------------------------
-- Entry point
-- ---------------------------------------------------------------------------

main :: IO ()
main = startApp App
  { initialAction = NoOp
  , model = Model
      { dslInput     = ""
      , output       = []
      , tileInputs   = []
      , newTileName  = ""
      , newTileShape = ""
      , vpZoom       = 1.0
      , vpPanX       = 0.0
      , vpPanY       = 0.0
      , isDragging   = False
      , lastMouseX   = 0
      , lastMouseY   = 0
      }
  , update     = updateModel
  , view       = viewModel
  , events     = defaultEvents
  , subs       = []
  , mountPoint = Nothing
  }

-- ---------------------------------------------------------------------------
-- Update
-- ---------------------------------------------------------------------------

updateModel :: Action -> Model -> Effect Action Model

updateModel (DSLInputChanged v) m = noEff m { dslInput = v }
updateModel (NewTileNameChanged v) m = noEff m { newTileName = v }
updateModel (NewTileShapeChanged v) m = noEff m { newTileShape = v }

updateModel AddTile m =
  noEff m { tileInputs = tileInputs m ++ [(newTileName m, newTileShape m)]
          , newTileName  = ""
          , newTileShape = ""
          }

updateModel (RemoveTile idx) m =
  noEff m { tileInputs = take idx (tileInputs m)
                       ++ drop (idx + 1) (tileInputs m) }

updateModel (SelectExample exName) m =
  case find (\ex -> exampleName ex == exName) examples of
    Just ex -> noEff m { dslInput   = exampleDSL ex
                       , tileInputs = exampleTiles ex
                       , vpZoom = 1.0, vpPanX = 0.0, vpPanY = 0.0
                       }
    Nothing -> noEff m

updateModel EvaluateDSL m = m <# do
  let code     = JS.unpack (dslInput m)
      tileData = map (\(n, s) -> (JS.unpack n, JS.unpack s)) (tileInputs m)
  result <- try (evaluate (force (runTslInBrowser code tileData)))
              :: IO (Either SomeException [String])
  case result of
    Left err   -> pure $ DSLResult (Left  (ms (show err)))
    Right outs -> pure $ DSLResult (Right (map ms outs))

updateModel (DSLResult res) m =
  case res of
    Left errMsg -> noEff m { output = ["Error: " <> errMsg] }
    Right outs  -> noEff m { output = outs
                           , vpZoom = 1.0, vpPanX = 0.0, vpPanY = 0.0
                           }

updateModel NoOp m = noEff m

-- Start drag ---------------------------------------------------------------
updateModel (StartDrag mx my) m =
  noEff m { isDragging = True, lastMouseX = mx, lastMouseY = my }

-- Drag (pan) — direct pixel mapping, no scaling needed --------------------
updateModel (DragMove mx my) m
  | not (isDragging m) = noEff m
  | otherwise =
    noEff m { vpPanX = vpPanX m + (mx - lastMouseX m)
            , vpPanY = vpPanY m + (my - lastMouseY m)
            , lastMouseX = mx
            , lastMouseY = my
            }

-- Stop drag ----------------------------------------------------------------
updateModel StopDrag m = noEff m { isDragging = False }

-- Wheel zoom — zoom towards cursor position -------------------------------
--   offsetX/Y is the cursor position within the container.
--   Formula: keep the SVG point under the cursor stationary.
--     screenPt = pan + offset           (not scaled, it's the div-local pos)
--     svgFrac  = (offset - pan) / zoom  (point in "unzoomed" SVG space)
--     After zoom:  newPan = offset - svgFrac * newZoom
updateModel (WheelZoom deltaY ox oy) m =
  let oldZ  = vpZoom m
      newZ  = clampZoom $ if deltaY < 0
                            then oldZ * zoomFactor
                            else oldZ / zoomFactor
      -- Keep the point under the cursor fixed
      newPX = ox - (ox - vpPanX m) * (newZ / oldZ)
      newPY = oy - (oy - vpPanY m) * (newZ / oldZ)
  in noEff m { vpZoom = newZ, vpPanX = newPX, vpPanY = newPY }

-- Reset --------------------------------------------------------------------
updateModel ResetViewport m =
  noEff m { vpZoom = 1.0, vpPanX = 0.0, vpPanY = 0.0 }

-- ---------------------------------------------------------------------------
-- View
-- ---------------------------------------------------------------------------

viewModel :: Model -> View Action
viewModel Model {..} = div_ [ style_ containerStyle ]
  [ div_ [ style_ leftColumnStyle ]
      [ h1_ [ style_ headerStyle ] [ text "Examples" ]
      , select_ [ onChange SelectExample, style_ dropdownStyle ]
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
          ] [ text dslInput ]
      , button_ [ onClick EvaluateDSL, style_ buttonStyle ]
          [ text "Evaluate DSL" ]
      , h2_ [ style_ headerStyle ] [ text "Tile Inputs:" ]
      , div_ [] (map (uncurry viewTile) (zip [0..] tileInputs))
      , h2_ [ style_ headerStyle ] [ text "Add New Tile:" ]
      , div_ [ style_ (mconcat [ "margin-bottom" =: "20px" ]) ]
          [ input_ [ placeholder_ "Tile Name"
                   , value_ newTileName
                   , onInput NewTileNameChanged
                   , style_ inputStyle ]
          , input_ [ placeholder_ "Tile Shape"
                   , value_ newTileShape
                   , onInput NewTileShapeChanged
                   , style_ inputStyle ]
          , button_ [ onClick AddTile, style_ buttonStyle ]
              [ text "Add Tile" ]
          ]
      ]
  , div_ [ style_ rightColumnStyle ]
      [ -- Toolbar
        div_ [ style_ (mconcat [ "display" =: "flex"
                                , "justify-content" =: "space-between"
                                , "align-items" =: "center"
                                , "margin-bottom" =: "8px" ]) ]
          [ h1_ [ style_ headerStyle ] [ text "Tile Output:" ]
          , div_ [ style_ (mconcat [ "display" =: "flex"
                                   , "gap" =: "6px"
                                   , "align-items" =: "center" ]) ]
              [ span_ [ style_ (mconcat [ "font-size" =: "13px"
                                        , "color" =: "#888" ]) ]
                  [ text (ms (show (round (vpZoom * 100) :: Int) ++ "%")) ]
              , button_ [ onClick (WheelZoom (-120) 300 200), style_ smallBtnStyle ]
                  [ text "+" ]
              , button_ [ onClick (WheelZoom 120 300 200), style_ smallBtnStyle ]
                  [ text "\x2212" ]
              , button_ [ onClick ResetViewport, style_ smallBtnStyle ]
                  [ text "Reset" ]
              ]
          ]
      , if not (null output) && ("Error:" `JS.isPrefixOf` head output)
          then div_ [ style_ errorStyle ] [ text (head output) ]
          else viewportContainer output vpZoom vpPanX vpPanY isDragging
      ]
  ]
  where
    containerStyle = mconcat
      [ "display" =: "flex", "flex-direction" =: "row"
      , "font-family" =: "Arial, sans-serif", "height" =: "100vh" ]
    leftColumnStyle = mconcat
      [ "width" =: "50%", "padding" =: "20px"
      , "background-color" =: "#f7f7f7"
      , "box-shadow" =: "2px 0px 5px rgba(0,0,0,0.1)"
      , "overflow-y" =: "auto" ]
    rightColumnStyle = mconcat
      [ "width" =: "50%", "padding" =: "20px"
      , "display" =: "flex", "flex-direction" =: "column" ]
    headerStyle   = mconcat [ "color" =: "#333", "margin-bottom" =: "10px" ]
    inputStyle    = mconcat [ "padding" =: "8px", "margin-right" =: "10px"
                            , "border" =: "1px solid #ccc"
                            , "border-radius" =: "4px" ]
    buttonStyle   = mconcat [ "padding" =: "10px 15px"
                            , "background-color" =: "#3498db"
                            , "color" =: "white", "border" =: "none"
                            , "border-radius" =: "4px"
                            , "cursor" =: "pointer"
                            , "margin-right" =: "10px" ]
    smallBtnStyle = mconcat [ "padding" =: "4px 10px"
                            , "background-color" =: "#3498db"
                            , "color" =: "white", "border" =: "none"
                            , "border-radius" =: "4px"
                            , "cursor" =: "pointer"
                            , "font-size" =: "14px" ]
    dropdownStyle = mconcat [ "width" =: "100%", "padding" =: "8px"
                            , "margin-bottom" =: "20px"
                            , "border" =: "1px solid #ccc"
                            , "border-radius" =: "4px" ]
    errorStyle    = mconcat [ "color" =: "red", "font-weight" =: "bold"
                            , "padding" =: "10px" ]

    defaultOption = option_ [ value_ "" ] [ text "Select an example" ]

    optionView :: Example -> View Action
    optionView ex = option_ [ value_ (exampleName ex) ]
                            [ text (exampleName ex) ]

    viewTile :: Int -> (MisoString, MisoString) -> View Action
    viewTile idx (name, shape) =
      div_ [ style_ (mconcat [ "margin-bottom" =: "10px"
                              , "padding" =: "5px"
                              , "background-color" =: "#fff"
                              , "border" =: "1px solid #ddd"
                              , "border-radius" =: "4px"
                              , "display" =: "flex"
                              , "justify-content" =: "space-between"
                              , "align-items" =: "center" ]) ]
        [ span_ [ style_ (mconcat [ "margin-right" =: "10px" ]) ]
            [ text ("Name: " <> name <> ", Shape: " <> shape) ]
        , button_ [ onClick (RemoveTile idx)
                  , style_ (mconcat [ "padding" =: "5px 10px"
                                    , "background-color" =: "#e74c3c"
                                    , "color" =: "white", "border" =: "none"
                                    , "border-radius" =: "4px"
                                    , "cursor" =: "pointer" ]) ]
            [ text "Remove" ]
        ]

-- | The viewport container.  The SVG content is rendered ONCE inside a
--   wrapper div.  Only the wrapper's CSS `transform` property changes on
--   zoom/pan — the browser GPU-accelerates this without re-laying-out the
--   thousands of SVG <rect> elements.
viewportContainer :: [MisoString] -> Float -> Float -> Float -> Bool -> View Action
viewportContainer outputLines zoom panX panY dragging =
  let tileData   = concatMap splitNewlines outputLines
      cursorVal  = if dragging then "grabbing" else "grab" :: String
      transformV = ms $ "translate(" ++ show panX ++ "px," ++ show panY ++ "px) "
                     ++ "scale(" ++ show zoom ++ ")"
  in div_
       [ style_ (mconcat [ "flex" =: "1"
                          , "border" =: "1px solid #ccc"
                          , "border-radius" =: "6px"
                          , "overflow" =: "hidden"
                          , "position" =: "relative"
                          , "cursor" =: ms cursorVal
                          , "user-select" =: "none"
                          , "min-height" =: "300px"
                          , "background-color" =: "#fafafa"
                          ])
       , on "mousedown"  mousePositionDecoder (\(x,y) -> StartDrag x y)
       , on "mousemove"  mousePositionDecoder (\(x,y) -> DragMove  x y)
       , on "mouseup"    mousePositionDecoder (\_ -> StopDrag)
       , on "mouseleave" mousePositionDecoder (\_ -> StopDrag)
       , onWithOptions (defaultOptions { preventDefault = True })
           "wheel" wheelDecoder (\(dy,ox,oy) -> WheelZoom dy ox oy)
       ]
       [ div_ [ style_ (mconcat [ "width" =: "100%"
                                 , "height" =: "100%"
                                 , "transform-origin" =: "0 0"
                                 , "transform" =: transformV
                                 , "will-change" =: "transform"
                                 ]) ]
           [ TileView.viewTileSVG tileData ]
       ]
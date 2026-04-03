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
import qualified Data.Map.Strict as Map

splitNewlines :: MisoString -> [MisoString]
splitNewlines s = map ms (lines (JS.unpack s))

examples :: [Example]
examples = [example1, example2, example3, example4, example5,
            example6, example7, example8, example9, example10,
            example11, example12, example13, example14, example15]

-- ---------------------------------------------------------------------------
-- Model
-- ---------------------------------------------------------------------------

data Model = Model
  { dslInput     :: MisoString
  , output       :: [MisoString]
  , tileInputs   :: [(MisoString, MisoString)]
  , newTileName  :: MisoString
  , newTileShape :: MisoString
  , vpZoom       :: Float
  , vpPanX       :: Float
  , vpPanY       :: Float
  , isDragging   :: Bool
  , lastMouseX   :: Float
  , lastMouseY   :: Float
  , isLoading    :: Bool
  , selectedDesc :: MisoString
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
  | StartDrag Float Float
  | DragMove  Float Float
  | StopDrag
  | WheelZoom Float Float Float
  | ResetViewport
  deriving (Show, Eq)

-- ---------------------------------------------------------------------------
-- Event decoders
-- ---------------------------------------------------------------------------

-- Mouse decoders (desktop)
mousePositionDecoder :: Decoder (Float, Float)
mousePositionDecoder = Decoder
  { decodeAt = DecodeTarget mempty
  , decoder  = withObject "MouseEvent" $ \o -> do
      x <- o .: "clientX"
      y <- o .: "clientY"
      return (x, y)
  }

wheelDecoder :: Decoder (Float, Float, Float)
wheelDecoder = Decoder
  { decodeAt = DecodeTarget mempty
  , decoder  = withObject "WheelEvent" $ \o -> do
      dy <- o .: "deltaY"
      ox <- o .: "offsetX"
      oy <- o .: "offsetY"
      return (dy, ox, oy)
  }

-- Touch decoders (mobile) — navigate into touches[0] / changedTouches[0]
touchStartDecoder :: Decoder (Float, Float)
touchStartDecoder = Decoder
  { decodeAt = DecodeTarget ["touches", "0"]
  , decoder  = withObject "Touch" $ \o -> do
      x <- o .: "clientX"
      y <- o .: "clientY"
      return (x, y)
  }

touchMoveDecoder :: Decoder (Float, Float)
touchMoveDecoder = Decoder
  { decodeAt = DecodeTarget ["touches", "0"]
  , decoder  = withObject "Touch" $ \o -> do
      x <- o .: "clientX"
      y <- o .: "clientY"
      return (x, y)
  }

touchEndDecoder :: Decoder ()
touchEndDecoder = Decoder
  { decodeAt = DecodeTarget mempty
  , decoder  = \_ -> return ()
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
      , isLoading    = False
      , selectedDesc = ""
      }
  , update     = updateModel
  , view       = viewModel
  , events     = Map.union defaultEvents $ Map.fromList
                    [ ("touchstart", False)
                    , ("touchmove",  False)
                    , ("touchend",   False)
                    , ("touchcancel", False)
                    ]
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
                       , selectedDesc = exampleDesc ex
                       , vpZoom = 1.0, vpPanX = 0.0, vpPanY = 0.0
                       }
    Nothing -> noEff m

updateModel EvaluateDSL m = m { isLoading = True } <# do
  let code     = JS.unpack (dslInput m)
      tileData = map (\(n, s) -> (JS.unpack n, JS.unpack s)) (tileInputs m)
  result <- try (evaluate (force (runTslInBrowser code tileData)))
              :: IO (Either SomeException [String])
  case result of
    Left err   -> pure $ DSLResult (Left  (ms (show err)))
    Right outs -> pure $ DSLResult (Right (map ms outs))

updateModel (DSLResult res) m =
  case res of
    Left errMsg -> noEff m { output = ["Error: " <> errMsg], isLoading = False }
    Right outs  -> noEff m { output = outs, isLoading = False
                           , vpZoom = 1.0, vpPanX = 0.0, vpPanY = 0.0
                           }

updateModel NoOp m = noEff m

updateModel (StartDrag mx my) m =
  noEff m { isDragging = True, lastMouseX = mx, lastMouseY = my }

updateModel (DragMove mx my) m
  | not (isDragging m) = noEff m
  | otherwise =
    noEff m { vpPanX = vpPanX m + (mx - lastMouseX m)
            , vpPanY = vpPanY m + (my - lastMouseY m)
            , lastMouseX = mx
            , lastMouseY = my
            }

updateModel StopDrag m = noEff m { isDragging = False }

updateModel (WheelZoom deltaY ox oy) m =
  let oldZ  = vpZoom m
      newZ  = clampZoom $ if deltaY < 0
                            then oldZ * zoomFactor
                            else oldZ / zoomFactor
      newPX = ox - (ox - vpPanX m) * (newZ / oldZ)
      newPY = oy - (oy - vpPanY m) * (newZ / oldZ)
  in noEff m { vpZoom = newZ, vpPanX = newPX, vpPanY = newPY }

updateModel ResetViewport m =
  noEff m { vpZoom = 1.0, vpPanX = 0.0, vpPanY = 0.0 }

-- ---------------------------------------------------------------------------
-- View
-- ---------------------------------------------------------------------------

viewModel :: Model -> View Action
viewModel Model {..} = div_ []
  [ -- Injected CSS: responsive layout + spinner + viewport meta
    node HTML (ms ("style" :: String)) Nothing []
      [ text responsiveCSS ]
  , div_ [ class_ "tttt-container" ]
      [ div_ [ class_ "tttt-editor" ]
          [ -- About section
            h1_ [ style_ (mconcat [ "color" =: "#222"
                                   , "margin-bottom" =: "4px"
                                   , "font-size" =: "22px" ]) ]
              [ text "The Terrible Tiling Toolkit" ]
          , p_ [ style_ (mconcat [ "color" =: "#666"
                                  , "font-size" =: "13px"
                                  , "line-height" =: "1.5"
                                  , "margin-bottom" =: "16px" ]) ]
              [ text "An experiment in building interpreters with Haskell. \
                     \This is a "
              , em_ [] [ text "domain-specific language" ]
              , text " (DSL) \x2014 a small, purpose-built programming language \
                     \designed to do one thing well: describe and transform \
                     \two-dimensional tile patterns. Unlike general-purpose \
                     \languages, a DSL trades breadth for expressiveness in its \
                     \target domain, letting you say in a few lines what would \
                     \take dozens in Python or JavaScript."
              ]
          , p_ [ style_ (mconcat [ "color" =: "#666"
                                  , "font-size" =: "13px"
                                  , "line-height" =: "1.5"
                                  , "margin-bottom" =: "16px" ]) ]
              [ text "Under the hood the source code is tokenised with "
              , a_ [ href_ "https://haskell-alex.readthedocs.io/"
                   , style_ linkStyle ] [ text "Alex" ]
              , text ", parsed with "
              , a_ [ href_ "https://haskell-happy.readthedocs.io/"
                   , style_ linkStyle ] [ text "Happy" ]
              , text ", and evaluated by a tree-walking interpreter \x2014 the \
                     \classic pipeline for exploring how programming languages \
                     \work. The browser front-end is made possible by the "
              , a_ [ href_ "https://haskell-miso.org/"
                   , style_ linkStyle ] [ text "Miso" ]
              , text " framework, which compiles Haskell to JavaScript via \
                     \GHCJS and provides an Elm-like architecture for building \
                     \reactive web apps entirely in Haskell. Huge thanks to the \
                     \Miso team for making this kind of thing feasible!"
              ]
          , hr_ [ style_ (mconcat [ "border" =: "none"
                                   , "border-top" =: "1px solid #ddd"
                                   , "margin-bottom" =: "16px" ]) ]

          , h2_ [ style_ sectionHeaderStyle ] [ text "Examples" ]
          , select_ [ onChange SelectExample, style_ dropdownStyle ]
              (defaultOption : map optionView examples)
          , if selectedDesc == ""
              then text ""
              else p_ [ style_ descStyle ] [ text selectedDesc ]
          , h2_ [ style_ sectionHeaderStyle ] [ text "DSL Editor" ]
          , textarea_
              [ onInput DSLInputChanged
              , style_ (mconcat [ "width" =: "100%"
                                , "height" =: "200px"
                                , "margin-bottom" =: "16px"
                                , "padding" =: "10px"
                                , "font-size" =: "16px"
                                , "box-sizing" =: "border-box"
                                ])
              ] [ text dslInput ]
          , button_ [ onClick EvaluateDSL, style_ buttonStyle ]
              [ text "Evaluate DSL" ]
          , h2_ [ style_ sectionHeaderStyle ] [ text "Tile Inputs:" ]
          , div_ [] (map (uncurry viewTile) (zip [0..] tileInputs))
          , h2_ [ style_ sectionHeaderStyle ] [ text "Add New Tile:" ]
          , div_ [ style_ (mconcat [ "margin-bottom" =: "20px"
                                   , "display" =: "flex"
                                   , "flex-wrap" =: "wrap"
                                   , "gap" =: "8px"
                                   ]) ]
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
      , div_ [ class_ "tttt-output" ]
          [ -- Toolbar
            div_ [ style_ (mconcat [ "display" =: "flex"
                                    , "justify-content" =: "space-between"
                                    , "align-items" =: "center"
                                    , "margin-bottom" =: "8px"
                                    , "flex-wrap" =: "wrap"
                                    , "gap" =: "8px"
                                    ]) ]
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
          , if isLoading
              then loadingIndicator
              else if not (null output) && ("Error:" `JS.isPrefixOf` head output)
                then div_ [ style_ errorStyle ] [ text (head output) ]
                else viewportContainer output vpZoom vpPanX vpPanY isDragging
          ]
      ]
  ]
  where
    headerStyle   = mconcat [ "color" =: "#333", "margin-bottom" =: "10px" ]
    sectionHeaderStyle = mconcat [ "color" =: "#333"
                                 , "margin-top" =: "20px"
                                 , "margin-bottom" =: "10px"
                                 , "font-size" =: "18px" ]
    descStyle     = mconcat [ "color" =: "#555", "font-size" =: "13px"
                            , "line-height" =: "1.4"
                            , "margin-top" =: "4px"
                            , "margin-bottom" =: "4px"
                            , "padding" =: "8px 12px"
                            , "background-color" =: "#eef4fb"
                            , "border-radius" =: "4px"
                            , "border-left" =: "3px solid #3498db"
                            ]
    linkStyle     = mconcat [ "color" =: "#2980b9", "text-decoration" =: "none" ]
    inputStyle    = mconcat [ "padding" =: "10px", "font-size" =: "16px"
                            , "border" =: "1px solid #ccc"
                            , "border-radius" =: "4px"
                            , "flex" =: "1", "min-width" =: "100px"
                            ]
    buttonStyle   = mconcat [ "padding" =: "12px 18px"
                            , "background-color" =: "#3498db"
                            , "color" =: "white", "border" =: "none"
                            , "border-radius" =: "4px"
                            , "cursor" =: "pointer"
                            , "font-size" =: "16px"
                            , "touch-action" =: "manipulation"
                            ]
    smallBtnStyle = mconcat [ "padding" =: "8px 14px"
                            , "background-color" =: "#3498db"
                            , "color" =: "white", "border" =: "none"
                            , "border-radius" =: "4px"
                            , "cursor" =: "pointer"
                            , "font-size" =: "16px"
                            , "touch-action" =: "manipulation"
                            ]
    dropdownStyle = mconcat [ "width" =: "100%", "padding" =: "10px"
                            , "margin-bottom" =: "20px", "font-size" =: "16px"
                            , "border" =: "1px solid #ccc"
                            , "border-radius" =: "4px" ]
    errorStyle    = mconcat [ "color" =: "red", "font-weight" =: "bold"
                            , "padding" =: "10px" ]

    loadingIndicator :: View Action
    loadingIndicator =
      div_ [ style_ (mconcat [ "flex" =: "1"
                              , "display" =: "flex"
                              , "flex-direction" =: "column"
                              , "justify-content" =: "center"
                              , "align-items" =: "center"
                              , "border" =: "1px solid #ccc"
                              , "border-radius" =: "6px"
                              , "background-color" =: "#fafafa"
                              , "min-height" =: "300px"
                              ]) ]
        [ div_ [ style_ (mconcat [ "width" =: "40px"
                                  , "height" =: "40px"
                                  , "border" =: "4px solid #e0e0e0"
                                  , "border-top" =: "4px solid #3498db"
                                  , "border-radius" =: "50%"
                                  , "animation" =: "spin 0.8s linear infinite"
                                  , "margin-bottom" =: "16px"
                                  ]) ] []
        , span_ [ style_ (mconcat [ "color" =: "#666"
                                   , "font-size" =: "14px" ]) ]
            [ text "Evaluating DSL..." ]
        ]

    defaultOption = option_ [ value_ "" ] [ text "Select an example" ]

    optionView :: Example -> View Action
    optionView ex = option_ [ value_ (exampleName ex) ]
                            [ text (exampleName ex) ]

    viewTile :: Int -> (MisoString, MisoString) -> View Action
    viewTile idx (name, shape) =
      div_ [ style_ (mconcat [ "margin-bottom" =: "10px"
                              , "padding" =: "8px"
                              , "background-color" =: "#fff"
                              , "border" =: "1px solid #ddd"
                              , "border-radius" =: "4px"
                              , "display" =: "flex"
                              , "justify-content" =: "space-between"
                              , "align-items" =: "center"
                              , "flex-wrap" =: "wrap"
                              , "gap" =: "8px"
                              ]) ]
        [ span_ [ style_ (mconcat [ "font-size" =: "14px"
                                   , "word-break" =: "break-all" ]) ]
            [ text ("Name: " <> name <> ", Shape: " <> shape) ]
        , button_ [ onClick (RemoveTile idx)
                  , style_ (mconcat [ "padding" =: "8px 14px"
                                    , "background-color" =: "#e74c3c"
                                    , "color" =: "white", "border" =: "none"
                                    , "border-radius" =: "4px"
                                    , "cursor" =: "pointer"
                                    , "font-size" =: "14px"
                                    , "touch-action" =: "manipulation"
                                    ]) ]
            [ text "Remove" ]
        ]

-- ---------------------------------------------------------------------------
-- Responsive CSS injected as a <style> element
-- ---------------------------------------------------------------------------

responsiveCSS :: MisoString
responsiveCSS = ms $ unlines
  [ "* { box-sizing: border-box; margin: 0; }"
  , "body { margin: 0; padding: 0; overflow-x: hidden; }"
  , "@keyframes spin { 0% { transform: rotate(0deg); } 100% { transform: rotate(360deg); } }"
  , ""
  , ".tttt-container {"
  , "  display: flex;"
  , "  flex-direction: row;"
  , "  font-family: Arial, sans-serif;"
  , "  height: 100vh;"
  , "}"
  , ".tttt-editor {"
  , "  width: 50%;"
  , "  padding: 20px;"
  , "  background-color: #f7f7f7;"
  , "  box-shadow: 2px 0px 5px rgba(0,0,0,0.1);"
  , "  overflow-y: auto;"
  , "}"
  , ".tttt-output {"
  , "  width: 50%;"
  , "  padding: 20px;"
  , "  display: flex;"
  , "  flex-direction: column;"
  , "}"
  , ""
  , "/* Mobile: stack vertically */"
  , "@media (max-width: 768px) {"
  , "  .tttt-container {"
  , "    flex-direction: column !important;"
  , "    height: auto !important;"
  , "    min-height: 100vh;"
  , "  }"
  , "  .tttt-editor {"
  , "    width: 100% !important;"
  , "    max-height: none;"
  , "    box-shadow: 0 2px 5px rgba(0,0,0,0.1) !important;"
  , "  }"
  , "  .tttt-output {"
  , "    width: 100% !important;"
  , "    min-height: 60vh;"
  , "  }"
  , "}"
  ]

-- ---------------------------------------------------------------------------
-- Viewport container with mouse + touch events
-- ---------------------------------------------------------------------------

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
                          , "touch-action" =: "none"
                          ])
       -- Mouse events (desktop)
       , on "mousedown"  mousePositionDecoder (\(x,y) -> StartDrag x y)
       , on "mousemove"  mousePositionDecoder (\(x,y) -> DragMove  x y)
       , on "mouseup"    mousePositionDecoder (\_ -> StopDrag)
       , on "mouseleave" mousePositionDecoder (\_ -> StopDrag)
       , onWithOptions (defaultOptions { preventDefault = True })
           "wheel" wheelDecoder (\(dy,ox,oy) -> WheelZoom dy ox oy)
       -- Touch events (mobile)
       , onWithOptions (defaultOptions { preventDefault = True })
           "touchstart" touchStartDecoder (\(x,y) -> StartDrag x y)
       , onWithOptions (defaultOptions { preventDefault = True })
           "touchmove" touchMoveDecoder (\(x,y) -> DragMove x y)
       , on "touchend"   touchEndDecoder (\_ -> StopDrag)
       , on "touchcancel" touchEndDecoder (\_ -> StopDrag)
       ]
       [ div_ [ style_ (mconcat [ "width" =: "100%"
                                 , "height" =: "100%"
                                 , "transform-origin" =: "0 0"
                                 , "transform" =: transformV
                                 , "will-change" =: "transform"
                                 ]) ]
           [ TileView.viewTileSVG tileData ]
       ]
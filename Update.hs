{-# LANGUAGE OverloadedStrings #-}
module Update (updateModel, examples) where

import Miso
import Miso.String (MisoString, ms)
import qualified Data.JSString as JS
import Control.Exception (try, SomeException, evaluate)
import Control.DeepSeq (force)
import Data.List (find)

import Types
import Examples
import Tsl (runTslInBrowser)

-- ---------------------------------------------------------------------------
-- Examples list
-- ---------------------------------------------------------------------------

examples :: [Example]
examples = [example1, example2, example3, example4, example5,
            example6, example7, example8, example9, example10,
            example11, example12, example13, example14, example15,
            example16]

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

-- Viewport: drag
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

-- Viewport: zoom towards cursor
updateModel (WheelZoom deltaY ox oy) m =
  let oldZ  = vpZoom m
      newZ  = clampZoom $ if deltaY < 0
                            then oldZ * zoomFactor
                            else oldZ / zoomFactor
      newPX = ox - (ox - vpPanX m) * (newZ / oldZ)
      newPY = oy - (oy - vpPanY m) * (newZ / oldZ)
  in noEff m { vpZoom = newZ, vpPanX = newPX, vpPanY = newPY }

-- Viewport: reset
updateModel ResetViewport m =
  noEff m { vpZoom = 1.0, vpPanX = 0.0, vpPanY = 0.0 }

-- Paper viewer
updateModel TogglePaper m =
  noEff m { showPaper = not (showPaper m) }
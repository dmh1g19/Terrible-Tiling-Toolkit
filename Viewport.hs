{-# LANGUAGE OverloadedStrings #-}
module Viewport (viewportContainer) where

import Miso
import Miso.String (MisoString, ms)
import Data.Monoid (mconcat)

import Types (Action(..), splitNewlines)
import Decoders
import qualified TileView

-- | The SVG viewport container with mouse + touch event handlers.
--   The SVG is rendered once; only the wrapper's CSS transform changes.
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

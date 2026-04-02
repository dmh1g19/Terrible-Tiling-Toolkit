{-# LANGUAGE OverloadedStrings #-}
module TileView where

import Miso
import Miso.String (MisoString, ms)
import qualified Data.JSString as JS
import qualified Miso.Svg as S
import qualified Miso.Svg.Attribute as A

-- | Size of each cell in pixels.
cellSize :: Float
cellSize = 10

-- | Viewport configuration for zoom and pan.
data ViewportConfig = ViewportConfig
  { vpZoom :: Float
  , vpPanX :: Float
  , vpPanY :: Float
  } deriving (Show, Eq)

-- | Default viewport: show everything, no pan.
defaultViewport :: ViewportConfig
defaultViewport = ViewportConfig 1.0 0.0 0.0

-- | Compute the natural content dimensions (width, height) in SVG units.
tileContentSize :: [MisoString] -> (Float, Float)
tileContentSize [] = (0, 0)
tileContentSize tile =
  let tileStrs = map JS.unpack tile
      numRows  = length tileStrs
      numCols  = if null tileStrs then 0 else length (head tileStrs)
  in (cellSize * fromIntegral numCols, cellSize * fromIntegral numRows)

-- | Render tile as an SVG that fills its container.
--   Accepts extra attributes so Main can attach mouse/wheel event handlers.
viewTileSVG :: [MisoString] -> ViewportConfig -> [Attribute action] -> View action
viewTileSVG [] _ _ = text "No tile data available."
viewTileSVG tile vp extraAttrs =
  let tileStrs = map JS.unpack tile
      (contentW, contentH) = tileContentSize tile
      vbW = contentW / vpZoom vp
      vbH = contentH / vpZoom vp
      vbX = vpPanX vp
      vbY = vpPanY vp
      viewBoxStr = ms $ show vbX ++ " " ++ show vbY ++ " "
                     ++ show vbW ++ " " ++ show vbH
  in S.svg_
       ([ A.width_ "100%", A.height_ "100%", A.viewBox_ viewBoxStr ] ++ extraAttrs)
       ( S.rect_
           [ A.x_ (ms (show vbX))
           , A.y_ (ms (show vbY))
           , A.width_ (ms (show vbW))
           , A.height_ (ms (show vbH))
           , A.fill_ "#f0f0f0"
           ] []
       : concatMap (\(y, row) -> map (cellRect y) (zip [0..] row))
                   (zip [0..] tileStrs)
       )
  where
    cellRect :: Int -> (Int, Char) -> View action
    cellRect y (x, c) =
      S.rect_
        [ A.x_ (ms (show (fromIntegral x * cellSize)))
        , A.y_ (ms (show (fromIntegral y * cellSize)))
        , A.width_ (ms (show cellSize))
        , A.height_ (ms (show cellSize))
        , A.fill_ (if c == '1' then "black" else "white")
        , A.stroke_ "gray"
        ]
        []
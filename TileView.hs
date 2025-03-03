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

-- | Given a tile (list of MisoString rows), produce an SVG element that renders it.
viewTileSVG :: [MisoString] -> View action
viewTileSVG [] = text "No tile data available."
viewTileSVG tile =
  let tileStrs = map JS.unpack tile  -- convert each row to a regular String
      numRows  = length tileStrs
      numCols  = if null tileStrs then 0 else length (head tileStrs)
      width    = cellSize * fromIntegral numCols
      height   = cellSize * fromIntegral numRows
      viewBoxStr = ms $ "0 0 " ++ show width ++ " " ++ show height
  in S.svg_
       [ A.width_  (ms (show width))
       , A.height_ (ms (show height))
       , A.viewBox_ viewBoxStr
       ]
       (concatMap (\(y, row) -> rowRects y row) (zip [0..] tileStrs))
  where
    rowRects :: Int -> String -> [View action]
    rowRects y rowStr = map (cellRect y) (zip [0..] rowStr)
    
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

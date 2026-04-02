{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BangPatterns #-}
module TileView where

import Miso
import Miso.String (MisoString, ms)
import qualified Data.JSString as JS
import qualified Miso.Svg as S
import qualified Miso.Svg.Attribute as A

-- | Size of each cell in pixels.
cellSize :: Float
cellSize = 10

-- | Render tile as an SVG with just 4 lightweight elements:
--   1. White background rect
--   2. Single <path> for ALL black cells
--   3. Horizontal grid lines (one <path>)
--   4. Vertical grid lines (one <path>)
--
--   Previously: one <rect> per cell = O(rows*cols) DOM nodes.
--   Now: always 4 DOM nodes regardless of tile size.
viewTileSVG :: [MisoString] -> View action
viewTileSVG [] = text "No tile data available."
viewTileSVG tile =
  let tileStrs = map JS.unpack tile
      numRows  = length tileStrs
      numCols  = if null tileStrs then 0 else length (head tileStrs)
      w        = cellSize * fromIntegral numCols
      h        = cellSize * fromIntegral numRows
      viewBoxStr = ms $ "0 0 " ++ show w ++ " " ++ show h
      -- Build a single SVG path for every black ('1') cell
      blackPathD = buildBlackPath cellSize tileStrs
      -- Build grid lines as two paths
      hGridD     = buildHGrid cellSize numRows w
      vGridD     = buildVGrid cellSize numCols h
  in S.svg_
       [ A.width_ "100%"
       , A.height_ "100%"
       , A.viewBox_ viewBoxStr
       ]
       [ -- 1. White background
         S.rect_
           [ A.x_ "0", A.y_ "0"
           , A.width_ (ms (show w))
           , A.height_ (ms (show h))
           , A.fill_ "white"
           ] []
       , -- 2. All black cells as one <path>
         S.path_
           [ A.d_ blackPathD
           , A.fill_ "black"
           ] []
       , -- 3. Horizontal grid lines
         S.path_
           [ A.d_ hGridD
           , A.fill_ "none"
           , A.stroke_ "#ccc"
           , A.strokeWidth_ "0.3"
           ] []
       , -- 4. Vertical grid lines
         S.path_
           [ A.d_ vGridD
           , A.fill_ "none"
           , A.stroke_ "#ccc"
           , A.strokeWidth_ "0.3"
           ] []
       ]

-- | Build the SVG path "d" attribute for all black cells.
--   Uses ShowS (difference lists) for O(1) append — avoids O(n^2) string concat.
buildBlackPath :: Float -> [String] -> MisoString
buildBlackPath cs rows = ms (go 0 rows "")
  where
    go :: Int -> [String] -> ShowS
    go _ []     = id
    go y (r:rs) = goRow y 0 r . go (y + 1) rs

    goRow :: Int -> Int -> String -> ShowS
    goRow _ _ []       = id
    goRow y x (c:rest)
      | c == '1'  = cellPath y x . goRow y (x + 1) rest
      | otherwise = goRow y (x + 1) rest

    -- Emit a tiny rect as a path: M x y h cs v cs h -cs Z
    cellPath :: Int -> Int -> ShowS
    cellPath y x =
      let !px = cs * fromIntegral x
          !py = cs * fromIntegral y
      in  showChar 'M' . shows px . showChar ' ' . shows py
        . showChar 'h' . shows cs
        . showChar 'v' . shows cs
        . showChar 'h' . shows (negate cs)
        . showChar 'Z'

-- | Horizontal grid lines: one "M0 y H w" per row boundary
buildHGrid :: Float -> Int -> Float -> MisoString
buildHGrid cs numRows w = ms (go 0 "")
  where
    go :: Int -> ShowS
    go i
      | i > numRows = id
      | otherwise   =
          let !y = cs * fromIntegral i
          in  showChar 'M' . showChar '0' . showChar ' ' . shows y
            . showChar 'H' . shows w
            . go (i + 1)

-- | Vertical grid lines: one "M x 0 V h" per col boundary
buildVGrid :: Float -> Int -> Float -> MisoString
buildVGrid cs numCols h = ms (go 0 "")
  where
    go :: Int -> ShowS
    go i
      | i > numCols = id
      | otherwise   =
          let !x = cs * fromIntegral i
          in  showChar 'M' . shows x . showString " 0"
            . showChar 'V' . shows h
            . go (i + 1)
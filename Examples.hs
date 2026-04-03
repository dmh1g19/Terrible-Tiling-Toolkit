{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}

module Examples where

import Miso
import Miso.String (MisoString, ms)

data Example = Example
  { exampleName  :: MisoString
  , exampleDesc  :: MisoString
  , exampleDSL   :: MisoString
  , exampleTiles :: [(MisoString, MisoString)]
  } deriving (Show, Eq)

example1 :: Example
example1 = Example
  { exampleName  = "Checker Board"
  , exampleDesc  = "Combines two single-pixel tiles and duplicates them into a classic checkerboard grid."
  , exampleDSL   = "getTileFile tile1 inp1;\ngetTileFile tile2 inp2;\n\ntile1 = combineTilesRight(inp1,inp2);\ntile2 = duplicateTileRight(tile1,32);\ntile3 = combineTilesDown(tile2, rotateTile180Degrees(tile2));\ntile4 = duplicateTileDown(tile3,32);\n\nreturn tile4;"
  , exampleTiles = [("tile1", "0"), ("tile2", "1")]
  }

example2 :: Example
example2 = Example
  { exampleName  = "Rotating and Scaling"
  , exampleDesc  = "Demonstrates rotation and scaling operations on a triangle tile to build a symmetric composition."
  , exampleDSL   = "getTileFile tile1 inp1;\n\ntile1 = squareRotateTile(inp1);\ntile2 = combineTilesRight(tile1,tile1);\ntile3 = combineTilesRight(tile2, (combineTilesDown(inp1,rotateTile270Degrees(inp1))));\ntile4 = combineTilesDown((tile3),(combineTilesRight((combineTilesDown(tile1,(combineTilesRight(inp1,(rotateTile90Degrees(inp1)))))),(scaleTile(inp1,3)))));\ntile5 = squareRotateTile(tile4);\n\nreturn tile5;"
  , exampleTiles = [("tile1", "0001 0011 0111 1111")]
  }

example3 :: Example
example3 = Example
  { exampleName  = "Reflections and blanks"
  , exampleDesc  = "Uses reflections and blank tiles to create a repeating wallpaper-style pattern."
  , exampleDSL   = "getTileFile tile1 inp1;\n\ntile1 = combineTilesRight((combineTilesRight(inp1,(reflectTileY(inp1)))), (createBlankTile(inp1)));\ntile2 = combineTilesRight((combineTilesRight((createBlankTile(inp1)),(reflectTileX(inp1)))), (reflectTileXY(inp1)));\nbaseTile = combineTilesDown(tile1,tile2);\ntile3 = combineTilesRight(baseTile,(reflectTileX(baseTile)));\ntile4 = combineTilesDown(tile3, (tile3));\ntile5 = duplicateTileRight(tile4, 10);\ntile6 = duplicateTileDown(tile5, 15);\nreturn tile6;"
  , exampleTiles = [("tile1", "000 000 011")]
  }

example4 :: Example
example4 = Example
  { exampleName  = "Boolean Ops and Predicates"
  , exampleDesc  = "Uses nested loops with conditionals and boolean tile operations to paint a diagonal split across a grid."
  , exampleDSL   = "getTileFile tile1 inp1;\ngetTileFile tile2 inp2;\ngetTileFile tile3 inp3;\n\n--Create a blank tile of length 50\nblankTile = createBlankTile(inp1);\nconjunct1Neg3 = conjunctTiles(inp1, negateTile(inp3));\nconjunct2Neg3 = conjunctTiles(inp2, negateTile(inp3));\n\nrow = 0;\ncolumn = 0;\n\nrowTile = duplicateTileRight(blankTile, 50);\ncolumnTile = createBlankTile(inp1);\n\nwhile (row < 50){\n\n\tif(((row + column) < 50) && (column < 25)){\n\t\tcolumnTile = conjunctTiles(inp1, negateTile(inp3));\n\t} else{\n\t\tif((row <= column) && (column >= 25)){\n\t\t\tcolumnTile = conjunctTiles(inp2, negateTile(inp3));\n\t\t} else{\n\t\t\tcolumnTile = createBlankTile(inp1);\n\t\t};\n\t};\n\tcolumn = column+1;\n\n\twhile(column < 50){\n\t\tif((row + column < 50) && (column < 25)){\n\t\t\tcolumnTile = combineTilesRight(columnTile, conjunctTiles(inp1, negateTile(inp3)));\n\t\t} else{\n\t\t\tif((row <= column) && (column >= 25)){\n\t\t\t\tcolumnTile = combineTilesRight(columnTile, conjunctTiles(inp2, negateTile(inp3)));\n\t\t\t} else{\n\t\t\t\tcolumnTile = combineTilesRight(columnTile, blankTile);\n\t\t\t};\n\t\t};\n\n\t\tcolumn = column+1;\n\t};\n\trowTile = combineTilesDown(rowTile, columnTile);\n\trow = row + 1;\n\tcolumn = 0;\n};\n\nfinalTile = removeTop(rowTile, 2);\n\nreturn finalTile;"
  , exampleTiles = [("tile1", "11 00"), ("tile2", "00 11"), ("tile3", "01 10")]
  }

example5 :: Example
example5 = Example
  { exampleName  = "Subtiles"
  , exampleDesc  = "Extracts sub-regions from a larger tile at different offsets and stacks them."
  , exampleDSL   = "getTileFile tile1 inp1;\n\ntile1 = duplicateTileRight( createSubTile(inp1,0,0,6,6), 3);\ntile2 = duplicateTileRight( createSubTile(inp1,2,2,6,6), 3);\ntile3 = duplicateTileRight( createSubTile(inp1,4,4,6,6), 3);\n\ntile4 = combineTilesDown(combineTilesDown(tile1,tile2),tile3);\nreturn tile4;"
  , exampleTiles = [("tile1", "1100000011 1100000011 0011001111 0011001111 0000110011 0000110011 0011001111 0011001111 1111111111 1111111111")]
  }

example6 :: Example
example6 = Example
  { exampleName  = "Three way alternation with chamfered corners"
  , exampleDesc  = "Builds a woven grid of three distinct tile types using nested loops with cycling logic."
  , exampleDSL   = "getTileFile tile1 inp1;\ngetTileFile tile2 inp2;\ngetTileFile tile3 inp3;\n\nblankTile = createBlankTile(inp1);\n\n-- First tile\ni = 0;\nfirstTile = combineTilesRight(blankTile, blankTile);\nwhile (i < 19) {\n\tfirstTile = combineTilesRight(firstTile, inp1);\n\tfirstTile = combineTilesRight(firstTile, inp2);\n\tfirstTile = combineTilesRight(firstTile, inp3);\n\ti = i + 1;\t\n};\nfirstTile = combineTilesRight(firstTile, inp1);\n\n-- Second Tile\ni = 0;\nsecondTile = createBlankTile(inp1);\nwhile (i < 19) {\n\tsecondTile = combineTilesRight(secondTile, inp2);\n\tsecondTile = combineTilesRight(secondTile, inp3);\n\tsecondTile = combineTilesRight(secondTile, inp1);\n\ti = i + 1;\t\n};\nsecondTile = combineTilesRight(secondTile, inp2);\nsecondTile = combineTilesRight(secondTile, inp3);\n\nmainTile = combineTilesDown(firstTile, secondTile);\n\nj = 1;\ni=0;\nwhile (i < 56) {\n\tif (j==1) {\n\t\ttempTile = combineTilesRight(inp3, inp1);\n\t\ttempTile = combineTilesRight(tempTile, inp2);\n\t\tk=0;\n\t\twhile (k<19) {\n\t\t\ttempTile = combineTilesRight(tempTile, inp3);\t\n\t\t\ttempTile = combineTilesRight(tempTile, inp1);\t\n\t\t\ttempTile = combineTilesRight(tempTile, inp2);\t\n\t\t\tk=k+1;\n\t\t};\n\t\tmainTile = combineTilesDown(mainTile, tempTile); \n\t} else { doNothing;};\n\n\tif (j==2) {\n\t\ttempTile = combineTilesRight(inp2, inp3);\n\t\ttempTile = combineTilesRight(tempTile, inp1);\n\t\tk=0;\n\t\twhile (k<19) {\n\t\t\ttempTile = combineTilesRight(tempTile, inp2);\t\n\t\t\ttempTile = combineTilesRight(tempTile, inp3);\t\n\t\t\ttempTile = combineTilesRight(tempTile, inp1);\t\n\t\t\tk=k+1;\n\t\t};\n\t\tmainTile = combineTilesDown(mainTile, tempTile); \n\t} else {doNothing;};\n\n\tif (j==3) {\n\t\ttempTile = combineTilesRight(inp1, inp2);\n\t\ttempTile = combineTilesRight(tempTile, inp3);\n\t\tk=0;\n\t\twhile (k<19) {\n\t\t\ttempTile = combineTilesRight(tempTile, inp1);\t\n\t\t\ttempTile = combineTilesRight(tempTile, inp2);\t\n\t\t\ttempTile = combineTilesRight(tempTile, inp3);\n\t\t\tk=k+1;\t\n\t\t};\n\t\tmainTile = combineTilesDown(mainTile, tempTile); \n\t\tj=0;\n\t}else {doNothing;};\n\tj=j+1;\n\ti=i+1;\n};\n\n-- Second last tile\nsecondLastTile = combineTilesRight(inp1, inp2);\nsecondLastTile = combineTilesRight(secondLastTile, inp3);\nk=0;\nwhile (k<19) {\n\tsecondLastTile = combineTilesRight(secondLastTile, inp1);\t\n\tsecondLastTile = combineTilesRight(secondLastTile, inp2);\n\tif(k!=18){\n\t\tsecondLastTile = combineTilesRight(secondLastTile, inp3);\n\t} else {doNothing;};\t\n\t\n\tk=k+1;\n};\nsecondLastTile = combineTilesRight(secondLastTile, blankTile);\n\n-- Last tile\nlastTile = combineTilesRight(inp3, inp1);\nk=0;\nwhile (k<19) {\n\tlastTile = combineTilesRight(lastTile, inp2);\n\tlastTile = combineTilesRight(lastTile, inp3);\n\tif(k!=18){\n\t\tlastTile = combineTilesRight(lastTile, inp1);\t\n\t} else {doNothing;};\n\tk=k+1;\n};\nlastTile = combineTilesRight(lastTile, blankTile);\nlastTile = combineTilesRight(lastTile, blankTile);\n\nmainTile = combineTilesDown(mainTile, secondLastTile);\nmainTile = combineTilesDown(mainTile, lastTile);\n\nreturn mainTile;"
  , exampleTiles = [("tile1", "111 111 111"), ("tile2", "010 111 010"), ("tile3", "101 010 101")]
  }

example7 :: Example
example7 = Example
  { exampleName  = "Repeated scaling"
  , exampleDesc  = "Overlays progressively smaller scaled copies using XOR to create a fractal zoom effect."
  , exampleDSL   = "S = 35;\n\ngetTileFile tile1 inp1;\n\nsubTilep1 = combineTilesRight(inp1, reflectTileY(inp1));\nsubTilep2 = combineTilesRight(reflectTileX(inp1), reflectTileXY(inp1));\nsubTile = combineTilesDown(subTilep1, subTilep2);\n\nblankRow = duplicateTileRight(createBlankTile(subTile), S);\noutputTile = scaleTile(subTile, S);\n\nx = S - 2;\nwhile(x > 0){\n\ttopRow = duplicateTileDown(blankRow, (S - x));\n\tleftPart = duplicateTileDown(duplicateTileRight(createBlankTile(subTile), (S - x)), x);\n\trightPart = scaleTile(subTile, x);\n\n\tcombinedLR = combineTilesRight(leftPart, rightPart);\n\tbigTile = combineTilesDown(topRow, combinedLR);\n\n\toutputTile = xorTiles(outputTile, bigTile);\n\tx = x - 2;\n};\n\nreturn outputTile;"
  , exampleTiles = [("tile1", "01 11")]
  }

example8 :: Example
example8 = Example
  { exampleName  = "Awkwardly composition"
  , exampleDesc  = "Manually composes dozens of tile fragments into an irregular, hand-crafted tessellation."
  , exampleDSL   = "getTileFile tile1 blue;\ngetTileFile tile2 green;\n\ngreen2Up = scaleTile(green,2);\ngreenDown = reflectTileX(green);\ngreen2Down = reflectTileX(green2Up);\nblueDown = reflectTileX (blue);\n\n\ntile1 = combineTilesRight( combineTilesRight(green2Down,green2Down) , combineTilesDown(greenDown,blue));\ntile2 = combineTilesDown( combineTilesDown(greenDown,blue), blue );\ntile3 = combineTilesDown(green2Down, combineTilesRight(blue,green));\ntile4 = combineTilesDown( combineTilesRight(blue,blue), green2Up);\ntile5 = combineTilesRight( combineTilesRight(tile2,tile3), tile4);\nsubTile1 = combineTilesDown(tile1,tile5);\n\n\ntile6 = combineTilesRight(combineTilesDown(greenDown,blue), combineTilesRight(green2Down,green2Down));\ntile7 = combineTilesDown(green2Down, combineTilesRight(green,blue));\ntile8 = combineTilesRight(tile4, combineTilesRight(tile7,tile2));\nsubTile2 = combineTilesDown(tile6,tile8);\n\n\ntile9 = combineTilesRight( combineTilesRight(blueDown,blueDown), greenDown);\ntile10 = combineTilesRight( combineTilesDown (blueDown,green), green2Up);\ntile11 = combineTilesRight( combineTilesDown (greenDown,blue), green2Down);\nsubTile3 = combineTilesDown( combineTilesDown (tile9,tile10), tile11);\n\n\ntile12 = combineTilesRight ( combineTilesRight(greenDown,blueDown), blueDown);\ntile13 = combineTilesRight (green2Up, combineTilesDown(blueDown,green));\ntile14 = combineTilesRight (green2Down, combineTilesDown(greenDown,blue));\nsubTile4 = combineTilesDown (combineTilesDown(tile12,tile13), tile14);\n\n\ntileSubRow1 = combineTilesRight(subTile3, subTile1);\ntileSubRow2 = combineTilesRight(subTile2, subTile4);\ntileSubRow3 = combineTilesRight(tileSubRow1, tileSubRow2);\ntileSubRowFinal = combineTilesRight(duplicateTileRight(tileSubRow3,2), combineTilesRight(subTile3,subTile1));\n\n\n\nfinalTiles1 = combineTilesDown(tileSubRowFinal, reflectTileX(tileSubRowFinal));\nfinalTile = duplicateTileDown(finalTiles1,4);\n\nreturn finalTile;"
  , exampleTiles = [("tile1", "010 001 110"), ("tile2", "010 101 101")]
  }

example9 :: Example
example9 = Example
  { exampleName  = "Diamond and gradients"
  , exampleDesc  = "Uses loops to build a triangular gradient, then combines rotations with XOR for a diamond shape."
  , exampleDSL   = "getTileFile tile1 inp1;\n\nrow = 0;\ncolumn = 0;\n\ncolumnTile = createBlankTile(inp1);\n\nif(2*column < row){\n\tcolumnTile = conjunctTiles(inp1,inp1);\n} else{\n\tcolumnTile = createBlankTile(inp1);\n};\ncolumn = column+1;\n\nwhile(column < 101){\n\tif((2*column) < row){\n\t\tcolumnTile = combineTilesRight(columnTile, inp1);\n\t} else{\n\t\tcolumnTile = combineTilesRight(columnTile, createBlankTile(inp1));\n    };\n    column = column+1;\n};\n\n\nrowTile = conjunctTiles(columnTile, columnTile);\nrow = row + 1;\ncolumn = 0;\n\nwhile (row < 101){\n\n\tif(2*column < row){\n\t\tcolumnTile = conjunctTiles(inp1,inp1);\n\t} else{\n\t\tcolumnTile = createBlankTile(inp1);\n\t};\n\tcolumn = column+1;\n\n\twhile(column < 101){\n\t\tif(2*column < row){\n\t\t    columnTile = combineTilesRight(columnTile, inp1);\n\t    } else{\n\t\t    columnTile = combineTilesRight(columnTile, createBlankTile(inp1));\n        };\n        column = column+1;\n\t};\n\trowTile = combineTilesDown(rowTile, columnTile);\n\trow = row + 1;\n\tcolumn = 0;\n};\n\ntileReflectY = reflectTileY(rowTile);\ntile270 = rotateTile270Degrees(rowTile);\ntile90Y = reflectTileY(rotateTile90Degrees(rowTile));\n\ntileP1 = xorTiles(rowTile, tile270);\ntileP2 = xorTiles(tileReflectY, tile90Y);\ntileP3 = xorTiles(tileP1, tileP2);\nreturn tileP3;"
  , exampleTiles = [("tile1", "010 101 010"), ("tile2", "00 11"), ("tile3", "01 10")]
  }

example10 :: Example
example10 = Example
  { exampleName  = "Partial replacement"
  , exampleDesc  = "Extracts regions from a tile and swaps in a different pattern using sub-tile operations."
  , exampleDSL   = "getTileFile tile1 inp1;\ngetTileFile tile2 inp2;\n\ntile1 = createSubTile(inp1,0,0,999999,12);\ntile2 = createSubTile(inp1,0,12,12,5);\ntile3 = createSubTile(inp1,17,12,999999,5);\ntile4 = createSubTile(inp1,0,17,999999,999999);\ntile5 = combineTilesRight(combineTilesRight(tile2,inp2),tile3);\ntile6 = combineTilesDown(combineTilesDown(tile1,tile5),tile4);\n\nreturn tile6;"
  , exampleTiles = [("tile1", "00000000000000000000 00000011111111000000 00000111111111100000 00001111111111110000 00011111111111111000 00111001111110011100 00111001111110011100 00111111111111111100 01111111111111111110 01111111111111111110 01111111111111111110 01111111111111111110 00111111111111111100 00111001111111001100 00111100000000011100 00011111111111111000 00001111111111110000 00000111111111100000 00000011111111000000 00000000000000000000"), ("tile2", "11111 10101 11011 10101 11111")]
  }

example11 :: Example
example11 = Example
  { exampleName  = "Fractal Pinwheel"
  , exampleDesc  = "Three nested square rotations turn a triangle into a self-similar fractal windmill."
  , exampleDSL   = "getTileFile tile1 inp1;\n\n-- Each squareRotateTile doubles the size\n-- and places all 4 rotations in a 2x2 grid.\n-- Three levels: 4x4 -> 8x8 -> 16x16 -> 32x32\nlevel1 = squareRotateTile(inp1);\nlevel2 = squareRotateTile(level1);\nlevel3 = squareRotateTile(level2);\n\n-- Tile it 2x2 for a bigger canvas\nrow = duplicateTileRight(level3, 2);\ngrid = duplicateTileDown(row, 2);\n\nreturn grid;"
  , exampleTiles = [("tile1", "0001 0011 0111 1111")]
  }

example12 :: Example
example12 = Example
  { exampleName  = "Moire Interference"
  , exampleDesc  = "XORs two grids at slightly different scales to produce a shimmering optical illusion."
  , exampleDSL   = "getTileFile tile1 inp1;\n\n-- Scale to two different sizes\nsmall = scaleTile(inp1, 3);\nlarge = scaleTile(inp1, 4);\n\n-- Tile both to the same total size (3*8 = 4*6 = 24 cells per axis)\nsmallGrid = duplicateTileRight(duplicateTileDown(small, 8), 8);\nlargeGrid = duplicateTileRight(duplicateTileDown(large, 6), 6);\n\n-- XOR the two grids to create interference\nresult = xorTiles(smallGrid, largeGrid);\n\nreturn result;"
  , exampleTiles = [("tile1", "01 10")]
  }

example13 :: Example
example13 = Example
  { exampleName  = "Arabesque Tessellation"
  , exampleDesc  = "Alternates a symmetric motif with its negative to create Islamic geometry-inspired tiles."
  , exampleDSL   = "getTileFile tile1 inp1;\n\n-- Build a 4-fold symmetric motif\nmotif = squareRotateTile(inp1);\n\n-- Alternate the motif with its inverse in a checkerboard\nrowA = combineTilesRight(motif, negateTile(motif));\nrowB = combineTilesRight(negateTile(motif), motif);\nblock = combineTilesDown(rowA, rowB);\n\n-- Scale up for visibility and tile\nscaled = scaleTile(block, 2);\nrow = duplicateTileRight(scaled, 4);\ngrid = duplicateTileDown(row, 4);\n\nreturn grid;"
  , exampleTiles = [("tile1", "001 011 110")]
  }

-- | Builds the classic Sierpinski carpet fractal by hand: at each level,
--   arrange 8 copies of the previous level around a blank centre.
example14 :: Example
example14 = Example
  { exampleName  = "Sierpinski Carpet"
  , exampleDesc  = "Builds the classic fractal by recursively arranging 8 copies around a hollow centre, three levels deep."
  , exampleDSL   = "getTileFile tile1 inp1;\n\n-- Level 1: 3x3 grid with hollow centre\nblank1 = createBlankTile(inp1);\nrow1a = combineTilesRight(combineTilesRight(inp1, inp1), inp1);\nrow1b = combineTilesRight(combineTilesRight(inp1, blank1), inp1);\nlevel1 = combineTilesDown(combineTilesDown(row1a, row1b), row1a);\n\n-- Level 2: replace each cell with level1 / blank\nblank2 = createBlankTile(level1);\nrow2a = combineTilesRight(combineTilesRight(level1, level1), level1);\nrow2b = combineTilesRight(combineTilesRight(level1, blank2), level1);\nlevel2 = combineTilesDown(combineTilesDown(row2a, row2b), row2a);\n\n-- Level 3\nblank3 = createBlankTile(level2);\nrow3a = combineTilesRight(combineTilesRight(level2, level2), level2);\nrow3b = combineTilesRight(combineTilesRight(level2, blank3), level2);\nlevel3 = combineTilesDown(combineTilesDown(row3a, row3b), row3a);\n\nreturn level3;"
  , exampleTiles = [("tile1", "1")]
  }

-- | Iteratively XORs a diamond shape with a 2x-scaled version of itself,
--   producing concentric diamond rings that grow more intricate each pass.
example15 :: Example
example15 = Example
  { exampleName  = "Concentric Diamonds"
  , exampleDesc  = "Iteratively XORs scaled diamond shapes to produce recursive concentric ring patterns."
  , exampleDSL   = "getTileFile tile1 inp1;\n\n-- Build a diamond from 4 reflections of a triangle\ntopHalf = combineTilesRight(inp1, reflectTileY(inp1));\nresult = combineTilesDown(topHalf, reflectTileX(topHalf));\n\n-- Iterate: scale up 2x, tile the smaller version to match,\n-- then XOR them. Each pass adds a new ring.\ni = 0;\nwhile (i < 3) {\n\tbig = scaleTile(result, 2);\n\ttiled = duplicateTileRight(duplicateTileDown(result, 2), 2);\n\tresult = xorTiles(tiled, big);\n\ti = i + 1;\n};\n\nreturn result;"
  , exampleTiles = [("tile1", "0001 0011 0111 1111")]
  }

-- | The showcase example: two independent fractal layers grown from
--   different seeds, woven together via XOR, then reflected into a
--   kaleidoscopic mandala.  64x64 output using loops, scaling, XOR,
--   rotation, reflection, and 4-fold symmetry.
example16 :: Example
example16 = Example
  { exampleName  = "Fractal Tapestry"
  , exampleDesc  = "The showcase: two fractal layers grown from a triangle and a diamond, woven together via XOR, then given a final fractal expansion for a 64x64 mandala of layered detail."
  , exampleDSL   = "getTileFile tile1 inp1;\ngetTileFile tile2 inp2;\n\n-- Build two 4-fold symmetric motifs directly\nlayer = squareRotateTile(inp1);\naccent = squareRotateTile(inp2);\n\n-- Fractal expansion: 2 iterations (8x8 -> 16 -> 32)\ni = 0;\nwhile (i < 2) {\n\tbig = scaleTile(layer, 2);\n\ttiled = duplicateTileRight(duplicateTileDown(layer, 2), 2);\n\tlayer = xorTiles(big, tiled);\n\n\tbigB = scaleTile(accent, 2);\n\ttiledB = duplicateTileRight(duplicateTileDown(accent, 2), 2);\n\taccent = xorTiles(bigB, rotateTile90Degrees(tiledB));\n\n\ti = i + 1;\n};\n\n-- Weave the two independent fractal layers\ntapestry = xorTiles(layer, accent);\n\n-- One final fractal expansion on the woven result (32 -> 64)\nbig = scaleTile(tapestry, 2);\ntiled = duplicateTileRight(duplicateTileDown(tapestry, 2), 2);\ntapestry = xorTiles(big, tiled);\n\nreturn tapestry;"
  , exampleTiles = [("tile1", "0001 0011 0111 1111"), ("tile2", "0110 1001 1001 0110")]
  }
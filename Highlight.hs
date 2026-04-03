{-# LANGUAGE OverloadedStrings #-}
module Highlight (highlightDSL) where

import Miso
import Miso.String (MisoString, ms)
import qualified Data.JSString as JS
import Data.Char (isAlpha, isAlphaNum, isDigit)
import Data.List (isPrefixOf)
import qualified Data.Set as Set

-- | Given DSL source code, produce a list of <span> elements with
--   syntax-highlighted colours.
highlightDSL :: MisoString -> [View action]
highlightDSL src = tokenize (JS.unpack src)

-- ---------------------------------------------------------------------------
-- Simple lexer that produces coloured <span> elements
-- ---------------------------------------------------------------------------

tokenize :: String -> [View action]
tokenize [] = []

-- Comments: -- to end of line
tokenize ('-':'-':rest) =
  let (comment, after) = span (/= '\n') rest
  in  colored commentColor ("--" ++ comment) : tokenize after

-- Numbers
tokenize (c:rest)
  | isDigit c =
    let (num, after) = span isDigit rest
    in  colored numberColor (c : num) : tokenize after

-- Identifiers and keywords
tokenize (c:rest)
  | isAlpha c =
    let (word, after) = span (\x -> isAlphaNum x || x == '_' || x == '\'') rest
        full = c : word
    in  colored (classifyWord full) full : tokenize after

-- Operators: &&, ||
tokenize ('&':'&':rest) = colored operatorColor "&&" : tokenize rest
tokenize ('|':'|':rest) = colored operatorColor "||" : tokenize rest

-- Single-char operators and punctuation
tokenize (c:rest)
  | c `elem` ("=+-*/^<>!" :: String) = colored operatorColor [c] : tokenize rest
  | c `elem` ("(){};," :: String)    = colored punctColor [c] : tokenize rest
  | otherwise                        = colored plainColor [c] : tokenize rest

-- ---------------------------------------------------------------------------
-- Classify a word into a colour
-- ---------------------------------------------------------------------------

classifyWord :: String -> MisoString
classifyWord w
  | w `Set.member` controlKeywords   = keywordColor
  | w `Set.member` tileKeywords      = tileFuncColor
  | w `Set.member` boolKeywords      = boolColor
  | otherwise                        = varColor

controlKeywords :: Set.Set String
controlKeywords = Set.fromList
  [ "if", "else", "while", "return", "print"
  , "getTileFile", "doNothing"
  ]

tileKeywords :: Set.Set String
tileKeywords = Set.fromList
  [ "combineTilesRight", "combineTilesDown"
  , "duplicateTileRight", "duplicateTileDown"
  , "rotateTile90Degrees", "rotateTile180Degrees", "rotateTile270Degrees"
  , "squareRotateTile", "scaleTile"
  , "createBlankTile", "createSubTile"
  , "reflectTileX", "reflectTileY", "reflectTileXY"
  , "conjunctTiles", "xorTiles", "negateTile"
  , "removeTop"
  ]

boolKeywords :: Set.Set String
boolKeywords = Set.fromList ["true", "false"]

-- ---------------------------------------------------------------------------
-- Colours
-- ---------------------------------------------------------------------------

commentColor, numberColor, keywordColor, tileFuncColor :: MisoString
boolColor, operatorColor, punctColor, varColor, plainColor :: MisoString

commentColor  = "#6a9955"   -- green
numberColor   = "#b5cea8"   -- light green
keywordColor  = "#c586c0"   -- purple
tileFuncColor = "#dcdcaa"   -- yellow
boolColor     = "#569cd6"   -- blue
operatorColor = "#d4d4d4"   -- light grey
punctColor    = "#808080"   -- grey
varColor      = "#9cdcfe"   -- light blue
plainColor    = "#d4d4d4"   -- default

-- | Create a coloured <span> for a token.
colored :: MisoString -> String -> View action
colored color content =
  span_ [ style_ ("color" =: color) ] [ text (ms content) ]

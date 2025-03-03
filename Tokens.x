{ 
module Tokens where 
}

%wrapper "posn"
$binary = 0-1 
$digit = 0-9   
-- digits 
$alpha = [a-zA-Z]    
-- alphabetic characters

tokens :-

  $white+       ; --Ignore white space
  "--".*        ; --Comments

  $digit+	{ (\p s -> TokenInt p (read s)) }
  doNothing  { (\p s -> TokenDoNothing p) } --Does nothing


  --Basic code syntax
  \=			{ (\p s -> TokenEq p ) }
  \(			{ (\p s -> TokenLParen p) }
  \)			{ (\p s -> TokenRParen p) }
  \{      { (\p s -> TokenLSquig p) }
  \}      { (\p s -> TokenRSquig p) }
  \;      { (\p s -> TokenSemiColon p) }
  \,      { (\p s -> TokenComma p) }

  --Boolean Logic
  true		{ (\p s -> TokenTrue p) }
  false   { (\p s -> TokenFalse p) }
  "&&"    { (\p s -> TokenAnd p) }
  "||"    { (\p s -> TokenOr p) }
  "!"     { (\p s -> TokenNot p) }
  \<			{ (\p s -> TokenLessThan p) }
  \>      { (\p s -> TokenMoreThan p) }


  --Integer Logic
  \+			{ (\p s -> TokenPlus p) }
  \-			{ (\p s -> TokenMinus p) }
  \*			{ (\p s -> TokenTimes p) }
  \/			{ (\p s -> TokenDiv p) }
  \^			{ (\p s -> TokenExponential p) }

  --If/While
  if			{ (\p s -> TokenIf p) }
  while		{ (\p s -> TokenWhile p) }
  else		{ (\p s -> TokenElse p) }


  --Tile Functions:
  combineTilesRight { (\p s -> TokenCTR p) }
  combineTilesDown  { (\p s -> TokenCTD p) }
  duplicateTileRight { (\p s -> TokenDTR p) }
  duplicateTileDown { (\p s -> TokenDTD p) }
  rotateTile90Degrees { (\p s -> TokenRT90 p) }
  rotateTile180Degrees { (\p s -> TokenRT180 p) }
  rotateTile270Degrees {(\p s -> TokenRT270 p) }
  squareRotateTile { (\p s -> TokenSR p) }
  scaleTile { (\p s -> TokenST p) }
  createBlankTile { (\p s -> TokenBlank p) }
  reflectTileX { (\p s -> TokenRTX p) }
  reflectTileY { (\p s -> TokenRTY p) }
  reflectTileXY { (\p s -> TokenRTXY p) }
  print { (\p s -> TokenPrint p) }
  getTileFile { (\p s -> TokenTileFile p) }
  createSubTile { (\p s -> TokenSubTile p) }
  negateTile { (\p s -> TokenNegate p) }
  conjunctTiles { (\p s -> TokenConjunct p) }
  removeTop { (\p s -> TokenRemoveTop p) }
  return { (\p s -> TokenReturnTile p) }

  --Variable Name
  $alpha [$alpha $digit \_ \’]*   { (\p s -> TokenTileVar p s) } 

{

-- The token type: 
data Token =
--  TokenTileLine AlexPosn String  |  
  TokenInt AlexPosn Int          | 
  TokenDoNothing AlexPosn        |
  
  
  --Code Syntax
  TokenEq AlexPosn               |
  TokenLParen AlexPosn           |
  TokenRParen AlexPosn           |
  TokenLSquig AlexPosn           |
  TokenRSquig AlexPosn           |
  TokenSemiColon AlexPosn        |
  TokenComma AlexPosn            |

  --Boolean logic
  TokenTrue AlexPosn             |
  TokenFalse AlexPosn            |
  TokenAnd AlexPosn              |
  TokenOr AlexPosn               |
  TokenNot AlexPosn              |
  TokenLessThan AlexPosn         |
  TokenMoreThan AlexPosn         |

  --Integer logic
  TokenPlus AlexPosn             |
  TokenMinus AlexPosn		         |
  TokenTimes AlexPosn		         |
  TokenDiv AlexPosn		           |
  TokenExponential AlexPosn	     |

  --If/While
  TokenIf AlexPosn               |
  TokenWhile AlexPosn		         |
  TokenElse AlexPosn             |

  --Tile Functions:
  TokenCTR AlexPosn              |
  TokenCTD AlexPosn              |
  TokenDTR AlexPosn              |
  TokenDTD AlexPosn              |
  TokenRT90 AlexPosn             |
  TokenRT180 AlexPosn            |
  TokenRT270 AlexPosn            |
  TokenSR AlexPosn               |
  TokenST AlexPosn               |
  TokenBlank AlexPosn            |
  TokenRTX AlexPosn              |
  TokenRTY AlexPosn              |
  TokenRTXY AlexPosn             |
  TokenPrint AlexPosn            | 
  TokenTileFile AlexPosn         |
  TokenSubTile AlexPosn          |
  TokenConjunct AlexPosn         |
  TokenNegate AlexPosn           |
  TokenRemoveTop AlexPosn        |
  TokenReturnTile AlexPosn       |

  --Variables
  TokenTileVar AlexPosn String

  deriving (Eq,Show) 

tokenPosn :: Token -> String
--tokenPosn (TokenTileLine  (AlexPn a l c) _) = show(l) ++ ":" ++ show(c)
tokenPosn (TokenInt  (AlexPn a l c) _) = show(l) ++ ":" ++ show(c)
tokenPosn (TokenDoNothing  (AlexPn a l c)) = show(l) ++ ":" ++ show(c)


--Code Syntax
tokenPosn (TokenEq  (AlexPn a l c)) = show(l) ++ ":" ++ show(c)
tokenPosn (TokenLParen (AlexPn a l c)) = show(l) ++ ":" ++ show(c)
tokenPosn (TokenRParen (AlexPn a l c)) = show(l) ++ ":" ++ show(c)
tokenPosn (TokenLSquig (AlexPn a l c)) = show(l) ++ ":" ++ show(c)
tokenPosn (TokenRSquig (AlexPn a l c)) = show(l) ++ ":" ++ show(c)
tokenPosn (TokenSemiColon (AlexPn a l c)) = show(l) ++ ":" ++ show(c)
tokenPosn (TokenComma (AlexPn a l c)) = show(l) ++ ":" ++ show(c)

--Boolean logic
tokenPosn (TokenTrue  (AlexPn a l c)) = show(l) ++ ":" ++ show(c)
tokenPosn (TokenFalse  (AlexPn a l c)) = show(l) ++ ":" ++ show(c)
tokenPosn (TokenAnd  (AlexPn a l c)) = show(l) ++ ":" ++ show(c)
tokenPosn (TokenOr  (AlexPn a l c)) = show(l) ++ ":" ++ show(c)
tokenPosn (TokenNot  (AlexPn a l c)) = show(l) ++ ":" ++ show(c)
tokenPosn (TokenLessThan  (AlexPn a l c)) = show(l) ++ ":" ++ show(c)
tokenPosn (TokenMoreThan  (AlexPn a l c)) = show(l) ++ ":" ++ show(c)

--Integer Logic
tokenPosn (TokenPlus  (AlexPn a l c)) = show(l) ++ ":" ++ show(c)
tokenPosn (TokenMinus  (AlexPn a l c)) = show(l) ++ ":" ++ show(c)
tokenPosn (TokenTimes  (AlexPn a l c)) = show(l) ++ ":" ++ show(c)
tokenPosn (TokenDiv  (AlexPn a l c)) = show(l) ++ ":" ++ show(c)
tokenPosn (TokenExponential  (AlexPn a l c)) = show(l) ++ ":" ++ show(c)

--If/While
tokenPosn (TokenIf (AlexPn a l c)) = show(l) ++ ":" ++ show(c)
tokenPosn (TokenWhile (AlexPn a l c)) = show(l) ++ ":" ++ show(c)
tokenPosn (TokenElse (AlexPn a l c)) = show(l) ++ ":" ++ show(c)

--Tile Functions
tokenPosn (TokenCTR (AlexPn a l c)) = show(l) ++ ":" ++ show(c)
tokenPosn (TokenCTD (AlexPn a l c)) = show(l) ++ ":" ++ show(c)
tokenPosn (TokenDTR (AlexPn a l c)) = show(l) ++ ":" ++ show(c)
tokenPosn (TokenDTD (AlexPn a l c)) = show(l) ++ ":" ++ show(c)
tokenPosn (TokenRT90 (AlexPn a l c)) = show(l) ++ ":" ++ show(c)
tokenPosn (TokenRT180 (AlexPn a l c)) = show(l) ++ ":" ++ show(c)
tokenPosn (TokenRT270 (AlexPn a l c)) = show(l) ++ ":" ++ show(c)
tokenPosn (TokenSR (AlexPn a l c)) = show(l) ++ ":" ++ show(c)
tokenPosn (TokenST (AlexPn a l c)) = show(l) ++ ":" ++ show(c)
tokenPosn (TokenBlank (AlexPn a l c)) = show(l) ++ ":" ++ show(c)
tokenPosn (TokenRTX (AlexPn a l c)) = show(l) ++ ":" ++ show(c)
tokenPosn (TokenRTY (AlexPn a l c)) = show(l) ++ ":" ++ show(c)
tokenPosn (TokenRTXY (AlexPn a l c)) = show(l) ++ ":" ++ show(c)
tokenPosn (TokenPrint (AlexPn a l c)) = show(l) ++ ":" ++ show(c)
tokenPosn (TokenTileFile (AlexPn a l c)) = show(l) ++ ":" ++ show(c)
tokenPosn (TokenSubTile (AlexPn a l c)) = show(l) ++ ":" ++ show(c)
tokenPosn (TokenRemoveTop (AlexPn a l c)) = show(l) ++ ":" ++ show(c)
tokenPosn (TokenReturnTile (AlexPn a l c)) = show(l) ++ ":" ++ show(c)

--Variables
tokenPosn (TokenTileVar (AlexPn a l c) _) = show(l) ++ ":" ++ show(c)

}
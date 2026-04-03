{-# LANGUAGE OverloadedStrings #-}
module Types where

import Miso
import Miso.String (MisoString, ms)
import qualified Data.JSString as JS

-- | Split a MisoString on newline characters.
splitNewlines :: MisoString -> [MisoString]
splitNewlines s = map ms (lines (JS.unpack s))

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
  , showPaper    :: Bool
  } deriving (Show, Eq)

-- | Initial model with all fields zeroed out.
initialModel :: Model
initialModel = Model
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
  , showPaper    = False
  }

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
  | TogglePaper
  deriving (Show, Eq)
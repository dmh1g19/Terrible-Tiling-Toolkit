{-# LANGUAGE OverloadedStrings #-}
module Decoders
  ( mousePositionDecoder
  , wheelDecoder
  , touchStartDecoder
  , touchMoveDecoder
  , touchEndDecoder
  ) where

import Miso
import Data.Aeson (withObject, (.:))

-- ---------------------------------------------------------------------------
-- Mouse decoders (desktop)
-- ---------------------------------------------------------------------------

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

-- ---------------------------------------------------------------------------
-- Touch decoders (mobile)
-- ---------------------------------------------------------------------------

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

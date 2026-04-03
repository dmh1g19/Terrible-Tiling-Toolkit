{-# LANGUAGE OverloadedStrings #-}

module Main where

import Miso
import qualified Data.Map.Strict as Map

import Types (Model, Action(..), initialModel)
import Update (updateModel)
import View (viewModel)

main :: IO ()
main = startApp App
  { initialAction = NoOp
  , model         = initialModel
  , update        = updateModel
  , view          = viewModel
  , events        = Map.union defaultEvents $ Map.fromList
                      [ ("touchstart", False)
                      , ("touchmove",  False)
                      , ("touchend",   False)
                      , ("touchcancel", False)
                      ]
  , subs          = []
  , mountPoint    = Nothing
  }

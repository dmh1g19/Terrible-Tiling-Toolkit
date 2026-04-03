{-# LANGUAGE OverloadedStrings #-}
module Styles where

import Miso
import Miso.String (MisoString, ms)
import Data.Monoid (mconcat)

-- ---------------------------------------------------------------------------
-- Shared styles used across View and Viewport
-- ---------------------------------------------------------------------------

headerStyle :: Attribute action
headerStyle = style_ $ mconcat [ "color" =: "#333", "margin-bottom" =: "10px" ]

sectionHeaderStyle :: Attribute action
sectionHeaderStyle = style_ $ mconcat
  [ "color" =: "#333"
  , "margin-top" =: "20px"
  , "margin-bottom" =: "10px"
  , "font-size" =: "18px"
  ]

descStyle :: Attribute action
descStyle = style_ $ mconcat
  [ "color" =: "#555", "font-size" =: "13px"
  , "line-height" =: "1.4"
  , "margin-top" =: "4px"
  , "margin-bottom" =: "4px"
  , "padding" =: "8px 12px"
  , "background-color" =: "#eef4fb"
  , "border-radius" =: "4px"
  , "border-left" =: "3px solid #3498db"
  ]

linkStyle :: Attribute action
linkStyle = style_ $ mconcat
  [ "color" =: "#2980b9", "text-decoration" =: "none" ]

inputStyle :: Attribute action
inputStyle = style_ $ mconcat
  [ "padding" =: "10px", "font-size" =: "16px"
  , "border" =: "1px solid #ccc"
  , "border-radius" =: "4px"
  , "flex" =: "1", "min-width" =: "100px"
  ]

buttonStyle :: Attribute action
buttonStyle = style_ $ mconcat
  [ "padding" =: "12px 18px"
  , "background-color" =: "#3498db"
  , "color" =: "white", "border" =: "none"
  , "border-radius" =: "4px"
  , "cursor" =: "pointer"
  , "font-size" =: "16px"
  , "touch-action" =: "manipulation"
  ]

smallBtnStyle :: Attribute action
smallBtnStyle = style_ $ mconcat
  [ "padding" =: "8px 14px"
  , "background-color" =: "#3498db"
  , "color" =: "white", "border" =: "none"
  , "border-radius" =: "4px"
  , "cursor" =: "pointer"
  , "font-size" =: "16px"
  , "touch-action" =: "manipulation"
  ]

dropdownStyle :: Attribute action
dropdownStyle = style_ $ mconcat
  [ "width" =: "100%", "padding" =: "10px"
  , "margin-bottom" =: "20px", "font-size" =: "16px"
  , "border" =: "1px solid #ccc"
  , "border-radius" =: "4px"
  ]

errorStyle :: Attribute action
errorStyle = style_ $ mconcat
  [ "color" =: "red", "font-weight" =: "bold"
  , "padding" =: "10px"
  ]

removeBtnStyle :: Attribute action
removeBtnStyle = style_ $ mconcat
  [ "padding" =: "8px 14px"
  , "background-color" =: "#e74c3c"
  , "color" =: "white", "border" =: "none"
  , "border-radius" =: "4px"
  , "cursor" =: "pointer"
  , "font-size" =: "14px"
  , "touch-action" =: "manipulation"
  ]

-- ---------------------------------------------------------------------------
-- Responsive CSS injected as a <style> element
-- ---------------------------------------------------------------------------

responsiveCSS :: MisoString
responsiveCSS = ms $ unlines
  [ "* { box-sizing: border-box; margin: 0; }"
  , "body { margin: 0; padding: 0; overflow-x: hidden; }"
  , "@keyframes spin { 0% { transform: rotate(0deg); } 100% { transform: rotate(360deg); } }"
  , ""
  , ".tttt-container {"
  , "  display: flex;"
  , "  flex-direction: row;"
  , "  font-family: Arial, sans-serif;"
  , "  height: 100vh;"
  , "}"
  , ".tttt-editor {"
  , "  width: 50%;"
  , "  padding: 20px;"
  , "  background-color: #f7f7f7;"
  , "  box-shadow: 2px 0px 5px rgba(0,0,0,0.1);"
  , "  overflow-y: auto;"
  , "}"
  , ".tttt-output {"
  , "  width: 50%;"
  , "  padding: 20px;"
  , "  display: flex;"
  , "  flex-direction: column;"
  , "}"
  , ""
  , "/* Mobile: stack vertically */"
  , "@media (max-width: 768px) {"
  , "  .tttt-container {"
  , "    flex-direction: column !important;"
  , "    height: auto !important;"
  , "    min-height: 100vh;"
  , "  }"
  , "  .tttt-editor {"
  , "    width: 100% !important;"
  , "    max-height: none;"
  , "    box-shadow: 0 2px 5px rgba(0,0,0,0.1) !important;"
  , "  }"
  , "  .tttt-output {"
  , "    width: 100% !important;"
  , "    min-height: 60vh;"
  , "  }"
  , "}"
  ]

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
module View (viewModel) where

import Miso
import Miso.String (MisoString, ms)
import qualified Data.JSString as JS
import Data.Monoid (mconcat)

import Types
import Styles
import Viewport (viewportContainer)
import Update (examples)
import Examples (Example(..))

import Highlight (highlightDSL)

-- ---------------------------------------------------------------------------
-- Top-level view
-- ---------------------------------------------------------------------------

viewModel :: Model -> View Action
viewModel Model {..} = div_ []
  [ -- Injected CSS
    node HTML (ms ("style" :: String)) Nothing []
      [ text responsiveCSS ]
  , div_ [ class_ "tttt-container" ]
      [ editorPanel
      , outputPanel
      ]
  , if showPaper then paperOverlay else text ""
  ]
  where
    -- =======================================================================
    -- Left panel: about, examples, editor, tile inputs
    -- =======================================================================
    editorPanel :: View Action
    editorPanel = div_ [ class_ "tttt-editor" ]
      [ aboutSection
      , hr_ [ style_ (mconcat [ "border" =: "none"
                                , "border-top" =: "1px solid #ddd"
                                , "margin-bottom" =: "16px" ]) ]
      , examplesSection
      , editorSection
      , tileInputsSection
      , addTileSection
      ]

    aboutSection :: View Action
    aboutSection = div_ []
      [ h1_ [ style_ (mconcat [ "color" =: "#222"
                                , "margin-bottom" =: "4px"
                                , "font-size" =: "22px" ]) ]
          [ text "The Terrible Tiling Toolkit" ]
      , p_ [ style_ (mconcat [ "color" =: "#666"
                              , "font-size" =: "13px"
                              , "line-height" =: "1.5"
                              , "margin-bottom" =: "16px" ]) ]
          [ text "An experiment in building interpreters with Haskell. \
                 \This is a "
          , em_ [] [ text "domain-specific language" ]
          , text " (DSL) \x2014 a small, purpose-built programming language \
                 \designed to do one thing well: describe and transform \
                 \two-dimensional tile patterns. Unlike general-purpose \
                 \languages, a DSL trades breadth for expressiveness in its \
                 \target domain, letting you say in a few lines what would \
                 \take dozens in Python or JavaScript."
          ]
      , p_ [ style_ (mconcat [ "color" =: "#666"
                              , "font-size" =: "13px"
                              , "line-height" =: "1.5"
                              , "margin-bottom" =: "16px" ]) ]
          [ text "Under the hood the source code is tokenised with "
          , a_ [ href_ "https://haskell-alex.readthedocs.io/"
               , linkStyle ] [ text "Alex" ]
          , text ", parsed with "
          , a_ [ href_ "https://haskell-happy.readthedocs.io/"
               , linkStyle ] [ text "Happy" ]
          , text ", and evaluated by a tree-walking interpreter \x2014 the \
                 \classic pipeline for exploring how programming languages \
                 \work. The browser front-end is made possible by the "
          , a_ [ href_ "https://haskell-miso.org/"
               , linkStyle ] [ text "Miso" ]
          , text " framework, which compiles Haskell to JavaScript via \
                 \GHCJS and provides an Elm-like architecture for building \
                 \reactive web apps entirely in Haskell. Huge thanks to the \
                 \Miso team for making this kind of thing feasible!"
          ]
      , p_ [ style_ (mconcat [ "color" =: "#666"
                              , "font-size" =: "13px"
                              , "line-height" =: "1.5"
                              , "margin-bottom" =: "16px" ]) ]
          [ text "For the full story behind this project, "
          , a_ [ onClick TogglePaper
               , style_ (mconcat [ "color" =: "#2980b9"
                                  , "text-decoration" =: "none"
                                  , "cursor" =: "pointer"
                                  , "border-bottom" =: "1px dashed #2980b9"
                                  ]) ]
              [ text "read the academic paper" ]
          , text "."
          ]
      ]

    examplesSection :: View Action
    examplesSection = div_ []
      [ h2_ [ sectionHeaderStyle ] [ text "Examples" ]
      , select_ [ onChange SelectExample, dropdownStyle ]
          (option_ [ value_ "" ] [ text "Select an example" ]
           : map optionView examples)
      , if selectedDesc == ""
          then text ""
          else p_ [ descStyle ] [ text selectedDesc ]
      ]

    optionView :: Example -> View Action
    optionView ex = option_ [ value_ (exampleName ex) ]
                            [ text (exampleName ex) ]

    editorSection :: View Action
    editorSection = div_ []
      [ h2_ [ sectionHeaderStyle ] [ text "DSL Editor" ]
      -- Outer div: fixed height, handles scrolling for BOTH layers
      , div_ [ style_ (mconcat [ "width" =: "100%"
                                , "height" =: "200px"
                                , "margin-bottom" =: "16px"
                                , "overflow" =: "auto"
                                , "border" =: "1px solid #444"
                                , "border-radius" =: "4px"
                                , "background-color" =: "#1e1e1e"
                                ]) ]
          [ -- Inner grid: both children occupy the same cell
            div_ [ style_ (mconcat [ "display" =: "grid"
                                    , "min-height" =: "100%"
                                    ]) ]
              [ -- Highlighted underlay
                node HTML (ms ("pre" :: String)) Nothing
                  [ style_ (editorBaseStyle <> mconcat
                      [ "grid-row" =: "1", "grid-column" =: "1"
                      , "color" =: "#d4d4d4"
                      , "pointer-events" =: "none"
                      ]) ]
                  (highlightDSL dslInput ++ [text "\n"])
              , -- Transparent textarea on top
                textarea_
                  [ onInput DSLInputChanged
                  , style_ (editorBaseStyle <> mconcat
                      [ "grid-row" =: "1", "grid-column" =: "1"
                      , "color" =: "transparent"
                      , "caret-color" =: "#d4d4d4"
                      , "resize" =: "none"
                      , "outline" =: "none"
                      , "-webkit-text-fill-color" =: "transparent"
                      ])
                  ] [ text dslInput ]
              ]
          ]
      , button_ [ onClick EvaluateDSL, buttonStyle ]
          [ text "Evaluate DSL" ]
      ]

    -- Shared CSS properties (as a Map) to keep pre and textarea aligned
    editorBaseStyle = mconcat
      [ "margin" =: "0"
      , "padding" =: "10px"
      , "font-family" =: "monospace"
      , "font-size" =: "14px"
      , "line-height" =: "1.4"
      , "white-space" =: "pre-wrap"
      , "word-wrap" =: "break-word"
      , "overflow" =: "hidden"
      , "box-sizing" =: "border-box"
      , "background" =: "transparent"
      , "border" =: "none"
      ]

    tileInputsSection :: View Action
    tileInputsSection = div_ []
      [ h2_ [ sectionHeaderStyle ] [ text "Tile Inputs:" ]
      , div_ [] (map (uncurry viewTile) (zip [0..] tileInputs))
      ]

    viewTile :: Int -> (MisoString, MisoString) -> View Action
    viewTile idx (name, shape) =
      div_ [ style_ (mconcat [ "margin-bottom" =: "10px"
                              , "padding" =: "8px"
                              , "background-color" =: "#fff"
                              , "border" =: "1px solid #ddd"
                              , "border-radius" =: "4px"
                              , "display" =: "flex"
                              , "justify-content" =: "space-between"
                              , "align-items" =: "center"
                              , "flex-wrap" =: "wrap"
                              , "gap" =: "8px"
                              ]) ]
        [ span_ [ style_ (mconcat [ "font-size" =: "14px"
                                   , "word-break" =: "break-all" ]) ]
            [ text ("Name: " <> name <> ", Shape: " <> shape) ]
        , button_ [ onClick (RemoveTile idx), removeBtnStyle ]
            [ text "Remove" ]
        ]

    addTileSection :: View Action
    addTileSection = div_ []
      [ h2_ [ sectionHeaderStyle ] [ text "Add New Tile:" ]
      , div_ [ style_ (mconcat [ "margin-bottom" =: "20px"
                                , "display" =: "flex"
                                , "flex-wrap" =: "wrap"
                                , "gap" =: "8px"
                                ]) ]
          [ input_ [ placeholder_ "Tile Name"
                   , value_ newTileName
                   , onInput NewTileNameChanged
                   , inputStyle ]
          , input_ [ placeholder_ "Tile Shape"
                   , value_ newTileShape
                   , onInput NewTileShapeChanged
                   , inputStyle ]
          , button_ [ onClick AddTile, buttonStyle ]
              [ text "Add Tile" ]
          ]
      ]

    -- =======================================================================
    -- Right panel: toolbar + output
    -- =======================================================================
    outputPanel :: View Action
    outputPanel = div_ [ class_ "tttt-output" ]
      [ toolbar
      , if isLoading
          then loadingIndicator
          else if not (null output) && ("Error:" `JS.isPrefixOf` head output)
            then div_ [ errorStyle ] [ text (head output) ]
            else viewportContainer output vpZoom vpPanX vpPanY isDragging
      ]

    toolbar :: View Action
    toolbar = div_ [ style_ (mconcat [ "display" =: "flex"
                                      , "justify-content" =: "space-between"
                                      , "align-items" =: "center"
                                      , "margin-bottom" =: "8px"
                                      , "flex-wrap" =: "wrap"
                                      , "gap" =: "8px" ]) ]
      [ h1_ [ headerStyle ] [ text "Tile Output:" ]
      , div_ [ style_ (mconcat [ "display" =: "flex"
                                , "gap" =: "6px"
                                , "align-items" =: "center" ]) ]
          [ span_ [ style_ (mconcat [ "font-size" =: "13px"
                                    , "color" =: "#888" ]) ]
              [ text (ms (show (round (vpZoom * 100) :: Int) ++ "%")) ]
          , button_ [ onClick (WheelZoom (-120) 300 200), smallBtnStyle ]
              [ text "+" ]
          , button_ [ onClick (WheelZoom 120 300 200), smallBtnStyle ]
              [ text "\x2212" ]
          , button_ [ onClick ResetViewport, smallBtnStyle ]
              [ text "Reset" ]
          ]
      ]

    loadingIndicator :: View Action
    loadingIndicator =
      div_ [ style_ (mconcat [ "flex" =: "1"
                              , "display" =: "flex"
                              , "flex-direction" =: "column"
                              , "justify-content" =: "center"
                              , "align-items" =: "center"
                              , "border" =: "1px solid #ccc"
                              , "border-radius" =: "6px"
                              , "background-color" =: "#fafafa"
                              , "min-height" =: "300px"
                              ]) ]
        [ div_ [ style_ (mconcat [ "width" =: "40px"
                                  , "height" =: "40px"
                                  , "border" =: "4px solid #e0e0e0"
                                  , "border-top" =: "4px solid #3498db"
                                  , "border-radius" =: "50%"
                                  , "animation" =: "spin 0.8s linear infinite"
                                  , "margin-bottom" =: "16px"
                                  ]) ] []
        , span_ [ style_ (mconcat [ "color" =: "#666"
                                   , "font-size" =: "14px" ]) ]
            [ text "Evaluating DSL..." ]
        ]

    -- =======================================================================
    -- PDF paper overlay
    -- =======================================================================
    paperOverlay :: View Action
    paperOverlay =
      div_ [ style_ (mconcat [ "position" =: "fixed"
                              , "top" =: "0", "left" =: "0"
                              , "width" =: "100vw", "height" =: "100vh"
                              , "background-color" =: "rgba(0,0,0,0.7)"
                              , "display" =: "flex"
                              , "flex-direction" =: "column"
                              , "align-items" =: "center"
                              , "justify-content" =: "center"
                              , "z-index" =: "9999"
                              ]) ]
        [ -- Header bar with title and close button
          div_ [ style_ (mconcat [ "width" =: "90%"
                                  , "max-width" =: "900px"
                                  , "display" =: "flex"
                                  , "justify-content" =: "space-between"
                                  , "align-items" =: "center"
                                  , "margin-bottom" =: "8px"
                                  ]) ]
            [ span_ [ style_ (mconcat [ "color" =: "#fff"
                                       , "font-size" =: "16px"
                                       , "font-family" =: "Arial, sans-serif" ]) ]
                [ text "Academic Paper \x2014 The Terrible Tiling Toolkit" ]
            , div_ [ style_ (mconcat [ "display" =: "flex"
                                      , "gap" =: "8px" ]) ]
                [ a_ [ href_ "TTT.pdf"
                     , textProp "target" "_blank"
                     , style_ (mconcat [ "color" =: "#fff"
                                        , "font-size" =: "14px"
                                        , "text-decoration" =: "none"
                                        , "padding" =: "6px 14px"
                                        , "border" =: "1px solid #fff"
                                        , "border-radius" =: "4px"
                                        , "font-family" =: "Arial, sans-serif"
                                        ]) ]
                    [ text "Open in new tab" ]
                , button_ [ onClick TogglePaper
                          , style_ (mconcat [ "padding" =: "6px 14px"
                                            , "background-color" =: "#e74c3c"
                                            , "color" =: "white"
                                            , "border" =: "none"
                                            , "border-radius" =: "4px"
                                            , "cursor" =: "pointer"
                                            , "font-size" =: "14px"
                                            ]) ]
                    [ text "Close" ]
                ]
            ]
        , -- PDF iframe
          node HTML (ms ("iframe" :: String)) Nothing
            [ textProp "src" "TTT.pdf"
            , style_ (mconcat [ "width" =: "90%"
                              , "max-width" =: "900px"
                              , "height" =: "85vh"
                              , "border" =: "none"
                              , "border-radius" =: "6px"
                              , "background-color" =: "#fff"
                              ]) ]
            []
        ]
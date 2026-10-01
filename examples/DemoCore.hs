{-# LANGUAGE OverloadedStrings #-}

-- | The demo view and model, without the window entry point, so the memory
-- benchmark runs the exact same UI headlessly.
module DemoCore
  ( Model (..)
  , demoModel
  , demo
  , gradientPixels
  ) where

import qualified Data.Text as T
import qualified Data.ByteString as BS
import Data.Word (Word8)
import ChibiUI

data Model = Model
  { name :: !Text
  , count :: !Int
  , temperature :: !Float
  , volume :: !Float
  , notes :: !Text
  , showPlot :: !Bool
  , wave :: !Wave
  , salutation :: !Text
  , tab :: !Int
  , locked :: !Bool
  }

data Wave = Sine | Saw
  deriving (Eq)

-- | The model the demo starts with.
demoModel :: Model
demoModel = Model "world" 0 20.5 0.4 "Write notes here.\nEnter adds a new line." True Sine "hello" 0 False

demo :: ChibiUI Model ()
demo = do
  label "chibi-ui tour"
  separator
  labelWrapped "Every widget chibi-ui has, in one window. Narrow the window to see this line wrap."
  space 4

  labeled "UI scale" $ do
    one <- button "1x"
    medium <- button "1.5x"
    two <- button "2x"
    auto <- button "Auto DPI"
    tooltip "Follow the monitor's scale"
    when one (setUiScale 1)
    when medium (setUiScale 1.5)
    when two (setUiScale 2)
    when auto (setUiScale 0)

  -- Buttons and state. Inside a row, children flow left to right.
  _ <- row $ do
    isLocked <- gets locked
    disabled isLocked $ do
      minus <- button "-"
      when minus (modify (\m -> m {count = count m - 1}))
      label . T.pack . show =<< gets count
      plus <- button "+"
      when plus (modify (\m -> m {count = count m + 1}))
    edit locked (\v m -> m {locked = v}) (checkbox "lock")

  separatorText "fields"

  -- Text and number fields. 'edit' shows part of the model in a widget
  -- and keeps what the widget returns.
  _ <- labeled "name" $ do
    fillWidth
    edit name (\v m -> m {name = v}) (textInputHint "your name")
  _ <- labeled "temp" $ do
    _ <- edit temperature (\v m -> m {temperature = v}) floatInput
    edit temperature (\v m -> m {temperature = v}) (`dragFloat` 0.1)
  labelDim "Drag to select; double-click a word; right-click to edit."
  labelDim "Tab: focus | Shift+F10: menu | Ctrl/Cmd+Z: undo"

  -- Slider and line plot.
  labeled "frequency" $ do
    nextWidth 180
    v <- edit volume (\v m -> m {volume = v}) (\x -> slider x 0 1)
    nextWidth 80
    progressBar v
  panel "plot" $ do
    shown <- edit showPlot (\v m -> m {showPlot = v}) (checkbox "show plot")
    vol <- gets volume
    shape <- edit wave (\v m -> m {wave = v}) (radio [("sine", Sine), ("saw", Saw)])
    let sample x = case shape of
          Sine -> sin x
          Saw -> x / pi - 2 * fromIntegral (floor (x / (2 * pi)) :: Int)
    when shown $ do
      nextWidth 220
      nextHeight 80
      plotLines [sample (fromIntegral i * 0.25 * (1 + vol)) | i <- [0 .. 59 :: Int]]

  newline
  hi <- edit salutation (\v m -> m {salutation = v})
    (combo [("hello", "hello"), ("hi", "hi"), ("howdy", "howdy")])
  let greeting = (\value -> hi <> ", " <> value <> "!") <$> gets name
  label =<< greeting
  contextMenu [("Copy greeting", setClipboard =<< greeting),
               ("Reset counter", modify (\m -> m {count = 0}))]

  space 4

  -- An image generated in code: a 48x48 gradient square.
  useImageRgba 0 48 48 1 (gradientPixels 48 48)
  image 0 48 48

  space 4

  -- Tabs pick which page shows below them; each page is one keyed group.
  page <- edit tab (\v m -> m {tab = v}) (tabs ["table", "tree", "notes"])
  column $ case page of
    0 -> withKey "table" $ do
      -- Table with row selection.
      picked <- table ["name", "age"] [["ada", "36"], ["grace", "45"], ["edsger", "72"]]
      newline
      labelDim (case picked of
                  Just i -> "selected row " <> T.pack (show (i :: Int))
                  Nothing -> "no row selected")
    1 -> withKey "tree" $
      treeNode "Project" $ do
        treeNode "Sources" $ do
          selectableText "Main.hs"
          selectableText "Widgets.hs"
        selectableText "README.md"
    _ -> withKey "notes" $ do
      _ <- edit notes (\v m -> m {notes = v}) textArea
      pure ()

  space 4
  labelDim "scrolled rows:"
  scrollColumn $ do
    mapM_ (\i -> label ("row " <> T.pack (show (i :: Int)))) [1 .. 40]

-- | A blue-to-white gradient, four bytes per pixel, top row first.
gradientPixels :: Int -> Int -> BS.ByteString
gradientPixels w h =
  BS.pack
    [ b
    | y <- [0 .. h - 1]
    , x <- [0 .. w - 1]
    , b <-
        [ fromIntegral ((x * 255) `div` max 1 (w - 1)) :: Word8
        , fromIntegral ((y * 255) `div` max 1 (h - 1))
        , 220
        , 255
        ]
    ]

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
  space 4

  row $ do
    alignTextToFrame
    label "UI scale"
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
  row $ do
    isLocked <- gets locked
    disabled isLocked $ do
      minus <- button "-"
      when minus (modify (\m -> m {count = count m - 1}))
      label . T.pack . show =<< gets count
      plus <- button "+"
      when plus (modify (\m -> m {count = count m + 1}))
    lock <- checkbox "lock" isLocked
    modify (\m -> m {locked = lock})

  space 4

  -- Text and number fields.
  row $ do
    alignTextToFrame
    label "name"
    nextWidth 180
    value <- textInput =<< gets name
    modify (\m -> m {name = value})
  row $ do
    alignTextToFrame
    label "temp"
    value <- floatInput =<< gets temperature
    modify (\m -> m {temperature = value})
  labelDim "Drag to select; double-click a word; right-click to edit."
  labelDim "Tab: focus | Shift+F10: menu | Ctrl/Cmd+Z: undo"

  -- Slider and line plot.
  row $ do
    alignTextToFrame
    label "frequency"
    nextWidth 180
    started <- gets volume
    v <- slider started 0 1
    modify (\m -> m {volume = v})
    nextWidth 80
    progressBar v
  shown <- checkbox "show plot" =<< gets showPlot
  modify (\m -> m {showPlot = shown})
  vol <- gets volume
  shape <- radio [("sine", Sine), ("saw", Saw)] =<< gets wave
  modify (\m -> m {wave = shape})
  let sample x = case shape of
        Sine -> sin x
        Saw -> x / pi - 2 * fromIntegral (floor (x / (2 * pi)) :: Int)
  when shown $ do
    nextWidth 220
    nextHeight 80
    plotLines [sample (fromIntegral i * 0.25 * (1 + vol)) | i <- [0 .. 59 :: Int]]

  newline
  hi <- combo [("hello", "hello"), ("hi", "hi"), ("howdy", "howdy")] =<< gets salutation
  modify (\m -> m {salutation = hi})
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
  page <- tabs ["table", "tree", "notes"] =<< gets tab
  modify (\m -> m {tab = page})
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
      editedNotes <- textArea =<< gets notes
      modify (\m -> m {notes = editedNotes})

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

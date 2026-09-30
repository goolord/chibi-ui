{-# LANGUAGE OverloadedStrings #-}

-- | The chibi-ui widget tour in a window: a counter row, text and number
-- fields, an image generated in code, a table, a nested tree, and a
-- scrolling column.
module Main (main) where

import ChibiUI.Backend.Rgfw
  ( RgfwOptions (..)
  , WindowSettings (..)
  , defaultRgfwOptions
  , defaultWindowSettings
  , runChibiApp
  )
import DemoCore (demo, demoModel)

main :: IO ()
main = runChibiApp opts demoModel demo
  where
    opts =
      defaultRgfwOptions
        { optWindow = defaultWindowSettings {wsTitle = "chibi-ui demo", wsSize = (520, 660)}
        }


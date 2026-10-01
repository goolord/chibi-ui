-- |
-- Module      : ChibiUI
-- Description : A tiny immediate-mode GUI on RGFW
-- Copyright   : (c) 2026 Zachary Churchill
-- License     : MIT
-- Maintainer  : zacharyachurchill@gmail.com
--
-- A view is a 'ChibiUI' action that runs every frame. Widgets are ordinary
-- calls placed at an advancing cursor: each one measures itself, draws at
-- the cursor, and steps the cursor past itself. There are no layout nodes
-- and no solver; a 'row' lays its body out left to right, 'sameLine'
-- keeps the next widget on the current line, and 'scrollColumn' clips and
-- scrolls a run of widgets.
--
-- > data Model = Model { count :: Int }
-- >
-- > counter :: ChibiUI Model ()
-- > counter = do
-- >   up <- button "+"
-- >   when up (modify (\m -> m { count = count m + 1 }))
-- >   sameLine
-- >   n <- gets count
-- >   label (T.pack (show n))
-- >   sameLine
-- >   down <- button "-"
-- >   when down (modify (\m -> m { count = count m - 1 }))
--
-- Run with @runChibiApp defaultRgfwOptions (Model 0) counter@ from
-- "ChibiUI.Backend.Rgfw".
--
-- = Conventions
--
-- * Widgets return what you usually need: 'Bool' for a button, the new
--   value for an input, the selected row for a table.
-- * Pass each input its current value and keep the result. Focused inputs
--   retain an editing draft and caret; unfocused inputs follow the supplied
--   value. Enter commits single-line fields and inserts a newline in 'textArea';
--   Escape restores the value from focus time.
-- * Application state is a user-defined model. 'get'/'gets' read it and
--   'put'/'modify' update it immediately, with changes retained across frames.
-- * Transient widget state (focus, drafts, scrolling) follows widget identity.
--   'withKey' keeps that identity stable when widgets reorder or disappear.
-- * 'nextWidth' and 'nextHeight' size the next widget, instead of its
--   measured one.
module ChibiUI
  ( -- * Views
    ChibiUI
  , liftIO
  , mapMsg

    -- * Application model
  , get
  , gets
  , put
  , modify
  , modify'
  , mapModel
  , edit

    -- * Widgets
  , label
  , labelDim
  , labeled
  , selectableText
  , button
  , checkbox
  , radio
  , combo
  , tabs
  , textInput
  , textInputHint
  , textArea
  , intInput
  , floatInput
  , slider
  , progressBar
  , image
  , useImageRgba
  , plotLines
  , table
  , treeNode
  , separator
  , separatorText
  , panel
  , contextMenu
  , tooltip
  , disabled

    -- * Layout

    -- | The cursor steps down after every widget. 'row' and 'column' scope a
    -- horizontal or vertical run; inside a 'row' children flow left to right
    -- on their own. 'sameLine' is the one-shot escape hatch for a pair, or
    -- for putting a helper-placed widget on the current line; 'newline'
    -- steps down explicitly; 'indent' shifts a body's lines; 'nextWidth'
    -- and 'nextHeight' size the next widget, and 'fillWidth' stretches it
    -- to the line's end; 'space' steps the cursor;
    -- 'scrollColumn' clips and scrolls its body to the window's bottom.
  , sameLine
  , newline
  , row
  , column
  , indent
  , nextWidth
  , fillWidth
  , nextHeight
  , space
  , scrollColumn
  , availWidth
  , textSize
  , measureText
  , alignTextToFrame
  , withTextAlign
  , TextAlign (..)

    -- * Input queries

    -- | 'keyPressed' and 'keyHeld' read the raw frame input unless a text
    -- field has the keyboard, so a focused field keeps its keys to itself.
  , getInput
  , mousePos
  , mousePressed
  , mouseReleased
  , mouseHeld
  , keyPressed
  , keyHeld
  , shortcut
  , primaryShortcut
  , noModifiers
  , uiScale
  , setUiScale
  , windowSize
  , windowWidth
  , uiTime
  , scrollDelta

    -- * Focus
  , isFocused
  , requestFocus
  , blurFocus
  , withKey

    -- * Last item

    -- | What happened to the widget or group placed just before: a
    -- 'row' or 'column' counts as one item, so these ask about the
    -- whole group.
  , itemRect
  , itemHovered
  , itemFocused
  , itemActive

    -- * Drawing
  , theme
  , withTheme
  , drawIO
  , fillRectUI
  , strokeRectUI
  , drawTextIn
  , drawTextAt
  , textInRect

    -- * Clipboard and frame control
  , getClipboard
  , setClipboard
  , requestFrame
  , requestFrameAt
  , quitUi

    -- * Re-exports
  , Text
  , Text.pack
  , Text.unpack
  , when
  , unless
  , Color
  , Rect (..)
  , Size (..)
  , V2 (..)
  , Key (..)
  , Modifiers (..)
  , MouseButton (..)
  , Theme (..)
  , defaultTheme
  , lightTheme
  , WidgetId
  ) where

import Control.Monad (unless, when)
import Data.Text (Text)
import qualified Data.Text as Text
import ChibiUI.Internal.Id (WidgetId)
import ChibiUI.Internal.Input (Key (..), Modifiers (..), MouseButton (..), noModifiers)
import ChibiUI.Internal.Menu (contextMenu, tooltip)
import ChibiUI.Internal.Monad
import ChibiUI.Internal.Style (Theme (..), TextAlign (..), defaultTheme, lightTheme)
import ChibiUI.Internal.Types (Color, Rect (..), Size (..), V2 (..))
import ChibiUI.Internal.Widgets
  ( button
  , checkbox
  , radio
  , combo
  , tabs
  , floatInput
  , image
  , intInput
  , label
  , labelDim
  , labeled
  , plotLines
  , progressBar
  , selectableText
  , scrollColumn
  , separator
  , separatorText
  , panel
  , slider
  , table
  , treeNode
  , textInput
  , textInputHint
  , textArea
  )

-- | Host and headless-test API: create a context with an initial model,
-- run scripted frames, and inspect their draw data and widget geometry.
module ChibiUI.Backend
  ( Context
  , newContext
  , runChibiUI
  , setTheme
  , setScale
  , withClipboard
  , setFont
  , contextInput
  , frameRects
  , Font
  , embeddedFont
  , newFont
  , fontSetScale
  , fontFree
  , runFrame
  , Damage (..)
  , FrameSnapshot
  , takeSnapshot
  , frameDamage
  , trackFrame
  , DrawData (..)
  , DrawCmd (..)
  , texFlat
  , texGlyphAtlas
  , texImage
  , Input (..)
  , Key (..)
  , Modifiers (..)
  , MouseButton (..)
  , MouseButtons
  , emptyInput
  , applyKey
  , applyMouseButton
  , applyPointerLeave
  , releaseAllKeys
  , keypadKey
  , clearEphemeral
  , inputInteracted
  , inputPointerHeld
  , pressedIn
  , releasedIn
  , heldIn
  , UiCursorKind (..)
  , cursorFallback
  , syncCursorKind
  , Color
  , Rect (..)
  , Size (..)
  , V2 (..)
  , ImageEntry (..)
  ) where

import ChibiUI.Internal.Context
import ChibiUI.Internal.Damage (Damage (..), FrameSnapshot, frameDamage, takeSnapshot, trackFrame)
import ChibiUI.Internal.Draw (DrawCmd (..), DrawData (..), texFlat, texGlyphAtlas, texImage)
import ChibiUI.Internal.Font (Font, embeddedFont, fontFree, fontSetScale, newFont)
import ChibiUI.Internal.Frame (runFrame)
import ChibiUI.Internal.Input
import ChibiUI.Internal.Monad (runChibiUI)
import ChibiUI.Internal.Types (Color, Rect (..), Size (..), V2 (..))

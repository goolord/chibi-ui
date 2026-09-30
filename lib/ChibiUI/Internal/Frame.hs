-- | The frame pipeline: reset the per-frame state, run the view, resolve
-- focus, and snapshot the draw list for the RGFW renderer.
module ChibiUI.Internal.Frame
  ( runFrame
  ) where

import Control.Monad (when)
import Data.IORef
import Data.List (elemIndex)
import Data.Maybe (fromMaybe)
import GHC.Clock (getMonotonicTime)
import ChibiUI.Internal.Context (Context (..), noWidget)
import ChibiUI.Internal.Draw
import ChibiUI.Internal.Id (WidgetId, initialIdContext)
import ChibiUI.Internal.Input
import ChibiUI.Internal.Monad
import ChibiUI.Internal.Menu (processPopup, paintPopup)
import ChibiUI.Internal.Layout (LayoutState (..), freshLayout)
import ChibiUI.Internal.Style (themeWindowPad)
import ChibiUI.Internal.Types (Rect (..), Size (..))

-- | Run one frame: move last frame's rects aside for hit tests, reset the
-- frame state, run the view, then resolve focus (Tab, click-to-unfocus)
-- and snapshot the draw list. The window background is the caller's clear;
-- the view draws everything else.
runFrame :: Context model -> Input -> ChibiUI model a -> IO (a, DrawData)
runFrame ctx inp view = do
  -- Last frame's geometry becomes the hit-test map; this frame records afresh.
  rects <- readIORef (ctxRects ctx)
  writeIORef (ctxPrevRects ctx) rects
  writeIORef (ctxRects ctx) mempty
  resetDrawArena (ctxArena ctx)
  pushClip (ctxArena ctx) (Rect 0 0 (sizeW (inputWindowSize inp)) (sizeH (inputWindowSize inp)))
  writeIORef (ctxFocusRequested ctx) False
  writeIORef (ctxFocusables ctx) []
  writeIORef (ctxCursor ctx) UiCursorDefault
  writeIORef (ctxFrameRequest ctx) False
  -- Widget ids count from the root again, so the same view derives the
  -- same ids every frame.
  writeIORef (ctxIdCtx ctx) initialIdContext
  writeIORef (ctxInput ctx) inp
  writeIORef (ctxInputBlocked ctx) False
  t <- getMonotonicTime
  writeIORef (ctxTime ctx) t
  -- Start the cursor at the window's content origin.
  theme0 <- readIORef (ctxTheme ctx)
  let pad = themeWindowPad theme0
      w = max 0 (sizeW (inputWindowSize inp) - pad * 2)
  writeIORef (ctxLayout ctx) freshLayout {lsPenX = pad, lsLineY = pad, lsIndent = pad, lsAvailW = w, lsLast = Rect pad pad 0 0}
  focusBefore <- readIORef (ctxFocus ctx)
  blocked <- runChibiUI ctx processPopup
  writeIORef (ctxInputBlocked ctx) blocked
  a <- runChibiUI ctx view
  -- The pointer grab outlives the widgets only while a button is held.
  -- Clearing after the view lets the active widget see its release this
  -- frame; a widget that stopped being declared cannot drop it itself.
  if inputPointerHeld inp then pure () else writeIORef (ctxActive ctx) noWidget
  -- Focus: Tab steps through this frame's focusables in declaration
  -- order; a click that no widget answered by claiming focus clears it.
  fs <- reverse <$> readIORef (ctxFocusables ctx)
  focusReq <- readIORef (ctxFocusRequested ctx)
  modifyIORef' (ctxFocus ctx) (resolveFocus inp blocked focusReq fs)
  focusAfter <- readIORef (ctxFocus ctx)
  -- Focus resolves after painting. Settle the old/new field visuals and
  -- drafts even when the user produces no further native event.
  when (focusAfter /= focusBefore) (writeIORef (ctxFrameRequest ctx) True)
  runChibiUI ctx (paintPopup inp)
  dd <- finishFrame (ctxArena ctx)
  pure (a, dd)

-- | Outside clicks win over Tab; blocked input still drops vanished widgets.
resolveFocus :: Input -> Bool -> Bool -> [WidgetId] -> WidgetId -> WidgetId
resolveFocus inp blocked requested fs focus
  | not blocked && not requested && pressedIn MouseLeft inp = noWidget
  | not blocked && pressedIn KeyTab inp && not (null fs) =
      fs !! ((fromMaybe start (elemIndex focus fs) + direction) `mod` length fs)
  | focus `elem` fs = focus
  | otherwise = noWidget
  where
    backwards = modShift (inputModifiers inp)
    direction = if backwards then -1 else 1
    start = if backwards then 0 else -1

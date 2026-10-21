-- | The frame pipeline: reset the per-frame state, run the view, resolve
-- focus, and snapshot the draw list for the RGFW renderer.
module ChibiUI.Internal.Frame
  ( runFrame
  ) where

import Control.Monad (unless, when)
import Data.IORef
import Data.List (elemIndex, (!?))
import Data.Maybe (fromMaybe)
import GHC.Clock (getMonotonicTime)

import ChibiUI.Internal.Context (Context (..), ScrollTarget (..), Wake (..), noWidget)
import ChibiUI.Internal.Draw
import ChibiUI.Internal.Id (WidgetId, initialIdPath)
import ChibiUI.Internal.Input
import ChibiUI.Internal.Layout (beginViewport)
import ChibiUI.Internal.Menu (paintPopup, paintTooltip, processPopup)
import ChibiUI.Internal.Monad
import ChibiUI.Internal.RectTable (clearRectTable)
import ChibiUI.Internal.Style (themeWindowPad)
import ChibiUI.Internal.Types (Rect (..), Size (..), V2 (..), rectArea, rectHit, rectInflate)

-- | Run one frame: reset the frame state, run the view, then resolve focus
-- (Tab, click-to-unfocus) and snapshot the draw list. The window
-- background is the caller's clear; the view draws everything else.
runFrame :: Context model -> Input -> ChibiUI model a -> IO (a, DrawData)
runFrame ctx inp view = do
  clearRectTable (ctxRects ctx)
  resetDrawArena (ctxArena ctx)
  let Size winW winH = inputWindowSize inp
      window = Rect 0 0 winW winH
  pushClip (ctxArena ctx) window
  writeIORef (ctxFocusRequested ctx) False
  writeIORef (ctxFocusables ctx) []
  writeIORef (ctxCursor ctx) UiCursorDefault
  writeIORef (ctxWake ctx) WakeIdle
  -- Widget ids count from the root again, so the same view derives the
  -- same ids every frame.
  writeIORef (ctxIdPath ctx) initialIdPath
  writeIORef (ctxIdSib ctx) 0
  writeIORef (ctxInput ctx) inp
  writeIORef (ctxInputBlocked ctx) False
  writeIORef (ctxTooltip ctx) Nothing
  t <- getMonotonicTime
  writeIORef (ctxTime ctx) t
  -- Start the cursor at the window's content origin.
  theme0 <- readIORef (ctxTheme ctx)
  writeIORef (ctxLayout ctx) (beginViewport (rectInflate (negate (themeWindowPad theme0)) window))
  focusBefore <- readIORef (ctxFocus ctx)
  blocked <- runChibiUI ctx processPopup
  writeIORef (ctxInputBlocked ctx) blocked
  -- The wheel goes to a region last frame declared; this frame's declare
  -- themselves afresh.
  targets <- readIORef (ctxScrollTargets ctx)
  writeIORef (ctxScrollTargets ctx) []
  writeIORef (ctxWheelOwner ctx) (if blocked then noWidget else wheelOwner inp targets)
  a <- runChibiUI ctx view
  -- The pointer grab outlives the widgets only while a button is held.
  -- Clearing after the view lets the active widget see its release this
  -- frame; a widget that stopped being declared cannot drop it itself.
  unless (inputPointerHeld inp) (writeIORef (ctxActive ctx) noWidget)
  -- Focus: Tab steps through this frame's focusables in declaration
  -- order; a click that no widget answered by claiming focus clears it.
  fs <- reverse <$> readIORef (ctxFocusables ctx)
  focusReq <- readIORef (ctxFocusRequested ctx)
  modifyIORef' (ctxFocus ctx) (resolveFocus inp blocked focusReq (map fst fs))
  focusAfter <- readIORef (ctxFocus ctx)
  writeIORef (ctxTyping ctx) $! lookup focusAfter fs == Just True

  -- Focus resolves after painting. Settle the old/new field visuals and
  -- drafts even when the user produces no further native event.
  runChibiUI ctx $ do
    when (focusAfter /= focusBefore) requestFrame
    paintTooltip >> paintPopup inp
  dd <- finishFrame (ctxArena ctx)
  pure (a, dd)

-- | The region this frame's wheel scrolls: the innermost one under the
-- pointer that can still move the wheel's way, so a region at its end
-- hands the wheel to the one around it. A nested region shows through its
-- parent's clip, so the innermost under the pointer is the smallest.
wheelOwner :: Input -> [ScrollTarget] -> WidgetId
wheelOwner inp targets =
  case [(rectArea s, wid) | ScrollTarget wid s o m <- targets, takes s o m] of
    [] -> noWidget
    t0 : ts -> snd $ foldl' (\a b -> if fst b < fst a then b else a) t0 ts
  where
    V2 dx dy = inputScroll inp
    takes shown (V2 x y) (V2 maxX maxY) =
      rectHit shown (inputMousePos inp)
        && ((dx < 0 && x > 0) || (dx > 0 && x < maxX) || (dy < 0 && y > 0) || (dy > 0 && y < maxY))

-- | Outside clicks win over Tab; blocked input still drops vanished widgets.
resolveFocus :: Input -> Bool -> Bool -> [WidgetId] -> WidgetId -> WidgetId
resolveFocus inp blocked requested fs focus
  | not blocked && not requested && pressedIn MouseLeft inp = noWidget
  | not blocked
  , pressedIn KeyTab inp
  , not (null fs)
  , let idx = (fromMaybe start (elemIndex focus fs) + direction) `mod` length fs
  , Just next <- fs !? idx =
      next
  | focus `elem` fs = focus
  | otherwise = noWidget
  where
    backwards = modShift (inputModifiers inp)
    direction = if backwards then -1 else 1
    start = if backwards then 0 else -1

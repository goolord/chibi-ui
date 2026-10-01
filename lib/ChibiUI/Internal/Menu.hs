-- | The overlays painted after the normal view: one flat, window-clamped
-- context menu, tooltips, and a debug overlay.
module ChibiUI.Internal.Menu
  ( contextMenu, openContextMenu, openPopup, processPopup, paintPopup
  , tooltip, paintTooltip, debugOverlay
  ) where

import Control.Monad (forM_, unless, when)
import Control.Monad.Reader (ask)
import Data.List (sortOn)
import Data.Maybe (fromMaybe, isJust, isNothing, listToMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import ChibiUI.Internal.Context (Context (..), Popup (..), frameRects, noWidget)
import ChibiUI.Internal.Draw (quadCount)
import ChibiUI.Internal.Font (lineHeight)
import ChibiUI.Internal.Id (WidgetId)
import ChibiUI.Internal.Input
import ChibiUI.Internal.Store (tooltipHovers)
import ChibiUI.Internal.Monad
import ChibiUI.Internal.Style
import ChibiUI.Internal.Types

-- | Attach a tooltip to the preceding widget or group: once the pointer has
-- been over it for half a second, @t@ shows in a box beside the pointer,
-- above every widget.
tooltip :: Text -> ChibiUI model ()
tooltip t = do
  wid <- nextId
  hov <- itemHovered
  since <- widgetState tooltipHovers wid
  now <- uiTime
  let delay = 0.5
  case (hov, since) of
    (False, Just _) -> setWidgetState tooltipHovers wid Nothing
    (True, Nothing) -> setWidgetState tooltipHovers wid (Just now) >> requestFrameAt (now + delay)
    (True, Just start)
      | now >= start + delay -> mousePos >>= \p -> writeCtx ctxTooltip (Just (p, t))
      | otherwise -> requestFrameAt (start + delay)
    _ -> pure ()

-- | Paint this frame's tooltip, below and right of the pointer, kept
-- inside the window.
paintTooltip :: ChibiUI model ()
paintTooltip = do
  current <- readCtx ctxTooltip
  forM_ current $ \(V2 px py, t) -> noteBox (V2 (px + 12) (py + 18)) t

-- | A line of text in a bordered box at a point, moved inside the window.
noteBox :: V2 -> Text -> ChibiUI model ()
noteBox (V2 x y) t = do
  win <- windowSize
  Size bw bh <- paddedText t
  let r = rectClampInto win (Rect x y bw bh)
  th <- theme
  withClip r $ do
    fillRectUI r (themeSurface th)
    strokeRectUI r 1 (themeBorder th)
    textInRect r t (themeText th)

-- | Outline every widget placed so far this frame, and label the smallest
-- one under the pointer with its rect; the top-right corner counts the
-- widgets and quads so far. Call it last in a view, to debug layout.
debugOverlay :: ChibiUI model ()
debugOverlay = do
  ctx <- ask
  rects <- map snd <$> liftIO (frameRects ctx)
  quads <- liftIO (quadCount (ctxArena ctx))
  p <- mousePos
  Size w _ <- windowSize
  forM_ rects $ \r -> strokeRectUI r 1 (colorRGBA 255 0 255 140)
  forM_ (listToMaybe (sortOn rectArea (filter (`rectHit` p) rects))) $ \r -> do
    fillRectUI r (colorRGBA 255 0 255 40)
    noteBox (rectBottomLeft r) $
      T.unwords [T.pack (show (round v :: Int)) | v <- [rectX r, rectY r, rectW r, rectH r]]
  noteBox (V2 w 0) (T.pack (show (length rects) ++ " widgets, " ++ show quads ++ " quads"))

-- | Attach a right-click menu to the preceding widget or group. Actions
-- should update application state, rather than declare more widgets.
-- Shift+F10 opens it while a widget inside it is focused; the first menu
-- declared wins, so a widget's own menu beats its group's. A 'disabled'
-- menu never opens.
contextMenu :: [(Text, ChibiUI model ())] -> ChibiUI model ()
contextMenu items = do
  wid <- nextId
  r <- itemRect
  recordRect wid r
  off <- isDisabled
  unless off (openContextMenu wid r [(t, True, action) | (t, action) <- items])

openContextMenu :: WidgetId -> Rect -> [(Text, Bool, ChibiUI model ())] -> ChibiUI model ()
openContextMenu wid r items = do
  inp <- getInput
  hov <- hovered r
  focus <- readCtx ctxFocus
  within <- holdsWithin r ctxFocus
  let focused = focus == wid || within
      keyboard = focused && modShift (inputModifiers inp) && pressedIn (KeyF 10) inp
  when (not (null items) && ((hov && pressedIn MouseRight inp) || keyboard)) $
    openPopup wid (if keyboard then rectBottomLeft r else inputMousePos inp) items

-- | Open a menu of @items@ at a point, for a widget: it closes when the
-- widget stops being declared. The first enabled item starts selected.
openPopup :: WidgetId -> V2 -> [(Text, Bool, ChibiUI model ())] -> ChibiUI model ()
openPopup wid position items = do
  ctx <- ask
  inp <- getInput
  let actions = [(t, enabled, runChibiUI ctx action) | (t, enabled, action) <- items]
      selected = firstOr (enabledIndices items)
  writeCtx ctxPopup (Just (Popup wid position actions selected (inputMousePos inp)))
  writeCtx ctxActive noWidget
  writeCtx ctxInputBlocked True
  requestFrame

-- | The positions of the enabled items.
enabledIndices :: [(a, Bool, b)] -> [Int]
enabledIndices items = [i | (i, (_, True, _)) <- zip [0 ..] items]

-- | The first index, or -1 for none.
firstOr :: [Int] -> Int
firstOr = fromMaybe (-1) . listToMaybe

geometry :: Popup -> ChibiUI model (Rect, Float)
geometry popup = do
  win@(Size w h) <- windowSize
  widths <- mapM (measureText . (\(t, _, _) -> t)) (popupItems popup)
  let V2 x y = popupPosition popup
      width = min w (maximum (120 : map (+ 20) widths))
      rowH = min (lineHeight + 10) (max 0 ((h - 2) / fromIntegral (length widths)))
      height = min h (2 + rowH * fromIntegral (length widths))
  pure (rectClampInto win (Rect x y width height), rowH)

rowAt :: Rect -> Float -> Input -> Maybe Int
rowAt r height inp
  | height > 0 && rectHit (r {rectY = rectY r + 1, rectH = max 0 (rectH r - 2)}) (inputMousePos inp) =
      Just (floor ((v2Y (inputMousePos inp) - rectY r - 1) / height))
  | otherwise = Nothing

-- | Consume the menu's input before widgets see it, including dismissal.
processPopup :: ChibiUI model Bool
processPopup = do
  current <- readCtx ctxPopup
  case current of
    Nothing -> pure False
    Just popup -> do
      inp <- getInput
      (r, rowH) <- geometry popup
      let (next, action) = stepPopup inp (rowAt r rowH inp) popup
      writeCtx ctxPopup next
      liftIO (sequence_ action)
      when (isNothing next) requestFrame
      pure True

-- | Decide the next popup and deferred action without running either effect.
stepPopup :: Input -> Maybe Int -> Popup -> (Maybe Popup, Maybe (IO ()))
stepPopup inp pointerRow popup = (next, action)
  where
    indices = enabledIndices (popupItems popup)
    step backwards =
      let order = if backwards then reverse indices else indices
          beyond = if backwards then (< popupSelected popup) else (> popupSelected popup)
       in firstOr (filter beyond order ++ order)
    selected | Just i <- firstPressed inp
                 [ (KeyDown, step False)
                 , (KeyUp, step True)
                 , (KeyHome, firstOr indices)
                 , (KeyEnd, firstOr (reverse indices))
                 ] = i
             | inputMousePos inp /= popupPointer popup,
               Just i <- pointerRow, i `elem` indices = i
             | otherwise = popupSelected popup
    clicked = pressedIn MouseLeft inp
    chosen | clicked = pointerRow
           | pressedIn KeyEnter inp = Just selected
           | otherwise = Nothing
    action = do
      i <- chosen
      (_, True, run) <- lookup i (zip [0 ..] (popupItems popup))
      pure run
    dismiss = clicked || pressedIn MouseRight inp || any (`pressedIn` inp) [KeyEscape, KeyTab]
    next | dismiss || isJust action = Nothing
         | otherwise = Just popup {popupSelected = selected, popupPointer = inputMousePos inp}

paintPopup :: Input -> ChibiUI model ()
paintPopup inp = do
  current <- readCtx ctxPopup
  forM_ current $ \popup -> do
    exists <- isJust <$> lookupWidgetRect (popupOwner popup)
    if not exists
      then writeCtx ctxPopup Nothing
      else do
        (r, rowH) <- geometry popup
        th <- theme
        withClip r $ do
          fillRectUI r (themeSurface th)
          forM_ (zip [0 ..] (popupItems popup)) $ \(i, (title, enabled, _)) -> do
            let rr = Rect (rectX r + 1) (rectY r + 1 + fromIntegral i * rowH) (max 0 (rectW r - 2)) rowH
                active = enabled && popupSelected popup == i
            when active (fillRectUI rr (themeSurfaceActive th))
            drawTextIn (rr {rectX = rectX rr + 8, rectW = max 0 (rectW rr - 16)}) title
              (if enabled then themeText th else themeTextDim th)
          strokeRectUI r 1 (themeBorder th)
        wantCursor (if rectHit r (inputMousePos inp) then UiCursorPointer else UiCursorDefault)

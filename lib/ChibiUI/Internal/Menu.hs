-- | One flat, window-clamped context menu, painted after the normal view.
module ChibiUI.Internal.Menu
  ( contextMenu, openContextMenu, openPopup, processPopup, paintPopup ) where

import Control.Monad (forM_, when)
import Data.Maybe (fromMaybe, isJust, isNothing, listToMaybe)
import Data.Text (Text)
import ChibiUI.Internal.Context (Context (..), Popup (..), noWidget)
import ChibiUI.Internal.Font (lineHeight)
import ChibiUI.Internal.Id (WidgetId)
import ChibiUI.Internal.Input
import ChibiUI.Internal.Layout (lsLast)
import ChibiUI.Internal.Monad
import ChibiUI.Internal.Style
import ChibiUI.Internal.Types

-- | Attach a right-click menu to the preceding widget or group. Actions
-- should update application state, rather than declare more widgets.
-- Shift+F10 opens it while a widget inside it is focused; the first menu
-- declared wins, so a widget's own menu beats its group's.
contextMenu :: [(Text, ChibiUI model ())] -> ChibiUI model ()
contextMenu items = do
  wid <- nextId
  r <- lsLast <$> readLayout
  recordRect wid r
  openContextMenu wid r [(t, True, action) | (t, action) <- items]

openContextMenu :: WidgetId -> Rect -> [(Text, Bool, ChibiUI model ())] -> ChibiUI model ()
openContextMenu wid r items = do
  inp <- getInput
  hov <- hovered r
  focus <- readCtx ctxFocus
  focusRect <- lookupWidgetRect focus
  let within (Rect x y w h) = x >= rectX r && y >= rectY r
        && x + w <= rectX r + rectW r && y + h <= rectY r + rectH r
      focused = focus == wid || (focus /= noWidget && maybe False within focusRect)
      keyboard = focused && modShift (inputModifiers inp) && pressedIn (KeyF 10) inp
  when (not (null items) && ((hov && pressedIn MouseRight inp) || keyboard)) $
    openPopup wid (if keyboard then V2 (rectX r) (rectY r + rectH r) else inputMousePos inp) items

-- | Open a menu of @items@ at a point, for a widget: it closes when the
-- widget stops being declared. The first enabled item starts selected.
openPopup :: WidgetId -> V2 -> [(Text, Bool, ChibiUI model ())] -> ChibiUI model ()
openPopup wid position items = do
  ctx <- askContext
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
  Size w h <- windowSize
  widths <- mapM (measureText . (\(t, _, _) -> t)) (popupItems popup)
  let V2 x y = popupPosition popup
      width = min w (maximum (120 : map (+ 20) widths))
      rowH = min (lineHeight + 10) (max 0 ((h - 2) / fromIntegral (length widths)))
      height = min h (2 + rowH * fromIntegral (length widths))
  pure (Rect (clamp 0 (max 0 (w - width)) x) (clamp 0 (max 0 (h - height)) y) width height, rowH)

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

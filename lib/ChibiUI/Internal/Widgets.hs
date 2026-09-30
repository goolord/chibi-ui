-- | The widgets. Each is an ordinary call: take an id, place a rect at the
-- cursor, read this frame's input, draw, return what the user did.
module ChibiUI.Internal.Widgets
  ( label
  , labelDim
  , selectableText
  , button
  , treeNode
  , textInput
  , textArea
  , intInput
  , floatInput
  , slider
  , image
  , plotLines
  , table
  , scrollColumn
  , separator
  ) where

import Control.Monad (foldM, forM, forM_, when, void)
import Data.Maybe (fromMaybe, isJust)
import Data.Dynamic (toDyn)
import Data.Text (Text)
import qualified Data.Text as T
import Text.Read (readMaybe)
import ChibiUI.Internal.Draw (emitQuadUV, texImage)
import ChibiUI.Internal.Font (lineHeight)
import ChibiUI.Internal.Id (WidgetId)
import ChibiUI.Internal.Input
import ChibiUI.Internal.Editor
import ChibiUI.Internal.Menu (openContextMenu)
import qualified ChibiUI.Internal.Layout as Layout
import ChibiUI.Internal.Monad
import ChibiUI.Internal.Store
import ChibiUI.Internal.Style
import ChibiUI.Internal.Types

-- | One line of text at the cursor.
label :: Text -> ChibiUI model ()
label = labelWith themeText

-- | One line of dimmed text, as a caption.
labelDim :: Text -> ChibiUI model ()
labelDim = labelWith themeTextDim

labelWith :: (Theme -> Color) -> Text -> ChibiUI model ()
labelWith color t = do
  (_, r) <- widgetRect (textSize t)
  th <- theme
  drawTextIn r t (color th)

-- Allocate identity before measuring; placement consumes size overrides once.
widgetRect :: ChibiUI model Size -> ChibiUI model (WidgetId, Rect)
widgetRect measure = do
  wid <- nextId
  r <- measure >>= place
  recordRect wid r
  pure (wid, r)

-- | A flat button with a focus border. 'True' on click, or Enter/Space
-- while keyboard-focused.
button :: Text -> ChibiUI model Bool
button t = do
  (wid, r) <- widgetRect $ do
    Size w h <- textSize t
    pure (Size (w + widgetPad * 2) (h + widgetPad * 2))
  th <- theme
  i <- interaction wid r
  let surface
        | iActive i = themeSurfaceActive th
        | iHovered i = themeSurfaceHover th
        | otherwise = themeSurface th
  fillRectUI r surface
  strokeRectUI r 1 (if iFocused i then themeAccent th else themeBorder th)
  textInRect r t (themeText th)
  pure (iClicked i)

-- | What the pointer and keyboard did to a pressable widget this frame.
data Interaction = Interaction {iHovered, iActive, iFocused, iClicked :: !Bool}

-- Buttons, tree headers, sliders and tables share Tab reachability, the
-- pointer grab a press takes (with the keyboard), and the pointer cursor.
-- A click is a release over the widget the press grabbed, or Enter/Space
-- while focused.
interaction :: WidgetId -> Rect -> ChibiUI model Interaction
interaction wid r = do
  addFocusable wid r False
  hov <- hovered r
  inp <- getInput
  when (hov && pressedIn MouseLeft inp) (void (claimActive wid) >> requestFocus wid)
  act <- isActive wid
  focused <- isFocused wid
  when (hov || act) (wantCursor UiCursorPointer)
  pure Interaction
    { iHovered = hov
    , iActive = act
    , iFocused = focused
    , iClicked = (releasedIn MouseLeft inp && act && hov) || (focused && any (`pressedIn` inp) [KeyEnter, KeySpace])
    }

-- | A collapsible branch, initially closed. Click or Enter/Space toggles;
-- Left closes and Right opens a focused header. Children run only while open.
-- Nest nodes freely; use 'withKey' when siblings can reorder. The branch is
-- one layout group, so opening it never changes the identity of later siblings.
treeNode :: Text -> ChibiUI model () -> ChibiUI model ()
treeNode title body = column $ do
  (wid, r) <- widgetRect $ do
    width <- availWidth
    pure (Size width (lineHeight + widgetPad * 2))
  let key = slotKey SlotTreeOpen (slotOf wid)
      gutter = lineHeight + widgetPad
  wasOpen <- storeRead (memberSlot fieldInt key)
  i <- interaction wid r
  inp <- getInput
  let open | iFocused i && pressedIn KeyLeft inp = False
           | iFocused i && pressedIn KeyRight inp = True
           | iClicked i = not wasOpen
           | otherwise = wasOpen
  when (open /= wasOpen) $ do
    storeUpdate (if open then insertSlot fieldInt key 1 else deleteSlot fieldInt key)
    requestFrame
  th <- theme
  withClip r $ do
    when (iHovered i || iActive i) (fillRectUI r (if iActive i then themeSurfaceActive th else themeSurfaceHover th))
    when (iFocused i) (strokeRectUI r 1 (themeAccent th))
    let x = rectX r + gutter / 2
        y = rectY r + rectH r / 2
    fillRectUI (Rect (x - 4.5) (y - 0.5) 9 1) (themeTextDim th)
    when (not open) (fillRectUI (Rect (x - 0.5) (y - 4.5) 1 9) (themeTextDim th))
    drawTextIn (r {rectX = rectX r + gutter, rectW = max 0 (rectW r - gutter)}) title (themeText th)
  when open (indent gutter body)

-- | A single-line text field. Pass the current value and keep the result:
-- while the field is focused it edits a draft seeded from the value and
-- returns the draft live; blur or Enter commits, and Escape restores the
-- focus-time value and blurs. Mouse dragging and Shift+movement select;
-- clipboard, word-motion and undo shortcuts share the right-click menu.
textInput :: Text -> ChibiUI model Text
textInput value = textField SingleLine (fieldSize 180) value (const id)

-- | A multiline field, five lines tall by default. Enter inserts a newline;
-- Tab moves focus and Escape restores the focus-time value. Lines do not wrap;
-- the viewport scrolls to the caret, or with the wheel while hovered.
textArea :: Text -> ChibiUI model Text
textArea value = textField MultiLine (cappedSize 320 (fieldHeight + lineHeight * 4)) (sanitizeText True value) (const id)

-- | A read-only, selectable/copyable line without field chrome.
selectableText :: Text -> ChibiUI model ()
selectableText value = void (textField ReadOnly measured value (const id))
  where
    measured = textSize value >>= \(Size w h) -> cappedSize w h

-- | An integer field. While focused, Up and Down step by 1, or by 10 with
-- Shift. A commit that does not parse reverts to the value passed in.
intInput :: Int -> ChibiUI model Int
intInput = numberInput

-- | A floating-point field, like 'intInput' with a step of 1 (10 with
-- Shift).
floatInput :: Float -> ChibiUI model Float
floatInput = numberInput

numberInput :: (Eq a, Num a, Read a, Show a) => a -> ChibiUI model a
numberInput value = do
  let shown = T.pack (show value)
  t <- textField SingleLine (fieldSize 120) shown (stepNumber value)
  -- An untouched field hands back the value it showed; skip the parse.
  pure (if t == shown then value else parseNumber value t)

parseNumber :: Read a => a -> Text -> a
parseNumber fallback = fromMaybe fallback . readMaybe . T.unpack . T.strip

fieldSize :: Float -> ChibiUI model Size
fieldSize width = cappedSize width fieldHeight

-- | @w@ by @h@, narrowed to the width left on the line.
cappedSize :: Float -> Float -> ChibiUI model Size
cappedSize w h = (\avail -> Size (min avail w) h) <$> availWidth

-- | A horizontal slider for @value@ between @lo@ and @hi@: drag the thumb
-- or click the track to set it, and Left/Right step a focused slider by a
-- tenth of the range. Returns the value it now holds.
slider :: Float -> Float -> Float -> ChibiUI model Float
slider value lo hi = do
  (wid, r) <- widgetRect (fieldSize 160)
  th <- theme
  i <- interaction wid r
  inp <- getInput
  let act = iActive i
      focused = iFocused i
      range = hi - lo
      frac v = if range > 0 then clamp01 ((v - lo) / range) else 0
      atFrac f = lo + f * range
      underPointer = clamp01 ((v2X (inputMousePos inp) - rectX r) / max 1 (rectW r))
      step = range / 10
      moved
        | act && heldIn MouseLeft inp = atFrac underPointer
        | focused && pressedIn KeyLeft inp = clamp lo hi (value - step)
        | focused && pressedIn KeyRight inp = clamp lo hi (value + step)
        | otherwise = value
      tw = min 8 (max 0 (rectW r))
      thumbX = rectX r + tw / 2 + frac moved * max 0 (rectW r - tw)
      cy = rectY r + rectH r / 2
      thumbH = min 14 (rectH r)
  fillRectUI (Rect (rectX r) (cy - 2) (rectW r) 4) (themeSurface th)
  fillRectUI (Rect (rectX r) (cy - 2) (max 0 (thumbX - rectX r)) 4) (themeAccent th)
  fillRectUI (Rect (thumbX - tw / 2) (cy - thumbH / 2) tw thumbH) (themeText th)
  when focused (strokeRectUI r 1 (themeAccent th))
  pure moved

data FieldMode = SingleLine | MultiLine | ReadOnly deriving (Eq)

-- | Space between a field's box and its text; read-only text has no box.
fieldInset :: FieldMode -> Float
fieldInset mode = if mode == ReadOnly then 0 else fieldPad

-- | Whether a field in this mode accepts an editing command, from the
-- keyboard or its menu: read-only text still moves, selects and copies.
commandAllowed :: FieldMode -> Command -> Bool
commandAllowed mode cmd = mode /= ReadOnly || case cmd of
  Move _ _ -> True
  SelectAll -> True
  Copy -> True
  _ -> False

-- An inactive editor retains undo history; an editing session also owns the
-- focus-time value. The draft and its cancellation target cannot drift apart.
data FieldState = Inactive !Editor | Editing !Text !Editor
  deriving (Eq)

data FieldEvent = BeginEdit !Text | UpdateEdit !Editor | CommitEdit | CancelEdit

fieldEditor :: FieldState -> Editor
fieldEditor = \case
  Inactive ed -> ed
  Editing _ ed -> ed

editing :: FieldState -> Bool
editing = \case
  Editing _ _ -> True
  Inactive _ -> False

stepField :: FieldEvent -> FieldState -> FieldState
stepField event state = case (event, state) of
  (BeginEdit value, Inactive ed) -> Editing value
    (if editText (editState ed) == value then ed else newEditor value)
  (UpdateEdit ed, Editing original _) -> Editing original ed
  (UpdateEdit ed, Inactive _) -> Inactive ed
  (CommitEdit, _) -> Inactive (fieldEditor state)
  (CancelEdit, Editing original _) -> Inactive (newEditor original)
  _ -> state

-- Stepping updates the same draft that is painted and subsequently edited.
stepNumber :: (Eq a, Num a, Read a, Show a) => a -> Input -> Text -> Text
stepNumber value inp t
  | delta == 0 = t
  | otherwise = T.pack (show (parseNumber value t + delta))
  where
    amount = if modShift (inputModifiers inp) then 10 else 1
    delta | pressedIn KeyUp inp = amount
          | pressedIn KeyDown inp = negate amount
          | otherwise = 0

-- | The shared editing lifecycle: seeding the draft on focus,
-- folding this frame's keys and text into it, drawing, and the value the
-- caller should keep.
textField :: FieldMode -> ChibiUI model Size -> Text -> (Input -> Text -> Text) -> ChibiUI model Text
textField mode measure value transform = do
  (wid, r) <- widgetRect measure
  addFocusable wid r True
  th <- theme
  let readOnly = mode == ReadOnly
      k = slotOf wid
  -- A click on the field takes the keyboard.
  hov <- hovered r
  pressed <- mousePressed
  inp <- getInput
  when (hov && (pressed || pressedIn MouseRight inp)) (requestFocus wid)
  focused <- isFocused wid
  saved <- storeRead (fromMaybe (Inactive (newEditor value)) . lookupDyn k)
  let hadDraft = editing saved
      started = if focused then stepField (BeginEdit value) saved else saved
      state = if readOnly && editText (editState (fieldEditor started)) /= value
        then stepField (UpdateEdit (newEditor value)) started else started
      ed0 = fieldEditor state
      paint ed active reveal = drawField mode k r ed active reveal th >> pure (editText (editState ed))
      finish next reveal = do
        let active = editing next
            reset = if active then id else
              deleteSlot fieldFloat (slotKey SlotTextScrollY k) . deleteSlot fieldFloat (slotKey SlotTextScroll k)
        -- An unchanged state keeps its slot: re-inserting an equal value
        -- rebuilds the store's map for nothing.
        when (next /= saved) $
          storeUpdate (insertSlot fieldDyn k (toDyn next) . reset)
        when (focused && not active) blurFocus
        paint (fieldEditor next) active reveal
  case (focused, state) of
    (False, Editing _ _) -> finish (stepField CommitEdit state) False
    (True, _) | pressedIn KeyEscape inp ->
      finish (if readOnly then Inactive (newEditor value) else stepField CancelEdit state) False
    (True, _) -> do
      edited <- editStep mode wid r ed0
      let draft = transform inp (editText (editState edited))
          ed = if draft == editText (editState edited) then edited else replace draft (command SelectAll edited)
          committed = mode /= MultiLine && pressedIn KeyEnter inp
          updated = stepField (UpdateEdit ed) state
      _ <- finish (if committed then stepField CommitEdit updated else updated)
        (not hadDraft || editState ed /= editState ed0 || (hov && pressed) || not (null (inputKeys inp)))
      let hasSelection = uncurry (/=) (selection ed)
          queue cmd = do
            requestFocus wid
            storeUpdate (insertSlot fieldDyn (slotKey SlotTextCommand k) (toDyn cmd))
      openContextMenu wid r
        [ (title, enabled, queue cmd)
        | (title, enabled, cmd) <-
            [ ("Undo", not (null (editUndo ed)), Undo)
            , ("Redo", not (null (editRedo ed)), Redo)
            , ("Cut", hasSelection, Cut)
            , ("Copy", hasSelection, Copy)
            , ("Paste", True, Paste)
            , ("Select all", not (T.null draft), SelectAll)
            ]
        , commandAllowed mode cmd
        ]
      pure draft
    (False, Inactive _) -> paint (newEditor value) False False

data ClickState = ClickState !Double !V2 !Int
data PointerStep = KeepSelection | DragSelection | ClickSelection !ClickState

-- Click sequences cycle caret -> word -> line/all -> caret. Only presses
-- advance the sequence; drags remain relative to the last press position.
stepPointer :: Input -> Bool -> Bool -> Double -> Maybe ClickState -> PointerStep
stepPointer inp hoveredField active now previous
  | hoveredField && pressedIn MouseLeft inp = ClickSelection (ClickState now position clicks)
  | active && moved && (heldIn MouseLeft inp || releasedIn MouseLeft inp) = DragSelection
  | otherwise = KeepSelection
  where
    position = inputMousePos inp
    delta p = let V2 x y = position `v2Sub` p in (abs x, abs y)
    moved = case previous of
      Just (ClickState _ p _) -> let (x, y) = delta p in x > 2 || y > 2
      Nothing -> False
    clicks = case previous of
      Just (ClickState time p count)
        | let (x, y) = delta p, now - time < 0.4, x < 4, y < 4 -> count `mod` 3 + 1
      _ -> 1

-- | Pointer selection and keyboard/menu commands all edit the same state.
editStep :: FieldMode -> WidgetId -> Rect -> Editor -> ChibiUI model Editor
editStep mode wid r ed0 = do
  inp <- getInput
  hov <- hovered r
  let multiline = mode == MultiLine
      k = slotOf wid
      pressed = hov && pressedIn MouseLeft inp
      t = editText (editState ed0)
  when pressed (void (claimActive wid))
  active <- isActive wid
  offset <- storeRead (findSlot fieldFloat 0 (slotKey SlotTextScroll k))
  offsetY <- storeRead (findSlot fieldFloat 0 (slotKey SlotTextScrollY k))
  previous <- storeRead (lookupDyn (slotKey SlotTextClick k))
  now <- uiTime
  when (active && heldIn MouseLeft inp && not (rectHit r (inputMousePos inp))) requestFrame
  ed1 <- case stepPointer inp hov active now previous of
    KeepSelection -> pure ed0
    gesture -> do
      let ls = textLines t
          lineIndex = clamp 0 (length ls - 1)
            (floor ((v2Y (inputMousePos inp) - rectY r - fieldPad + offsetY) / lineHeight))
          (start, line) = if multiline then ls !! lineIndex else (0, t)
      caret <- (start +) <$> hitCaret line (v2X (inputMousePos inp) - rectX r - fieldInset mode + offset)
      let anchor = if pressed && not (modShift (inputModifiers inp)) then caret else editAnchor (editState ed0)
          pointed = select anchor caret ed0
      case gesture of
        ClickSelection click@(ClickState _ _ clicks) -> do
          storeUpdate (insertSlot fieldDyn (slotKey SlotTextClick k) (toDyn click))
          pure $ case clicks of
            2 -> selectWord caret pointed
            3 | multiline -> select start (start + T.length line) pointed
              | otherwise -> command SelectAll pointed
            _ -> pointed
        _ -> pure pointed
  pending <- storeRead (lookupDyn (slotKey SlotTextCommand k))
  when (isJust pending) (storeUpdate (deleteSlot fieldDyn (slotKey SlotTextCommand k)))
  foldM (runEdit multiline) ed1 (filter (commandAllowed mode) (maybe [] (: []) pending ++ inputCommands multiline inp))

runEdit :: Bool -> Editor -> Command -> ChibiUI model Editor
runEdit multiline ed cmd = case cmd of
  Copy -> copy >> pure ed
  Cut -> copy >> pure (replace "" ed)
  Paste -> maybe ed (\t -> command (Insert (sanitizeText multiline t)) ed) <$> getClipboard
  _ -> pure (command cmd ed)
  where
    copy = when (uncurry (/=) (selection ed)) (setClipboard (selectedText ed))

-- Binary search measured prefixes, so proportional fonts and Unicode code
-- points use the same advances for hit testing and drawing.
hitCaret :: Text -> Float -> ChibiUI model Int
hitCaret t x = search 0 (T.length t)
  where
    search lo hi
      | lo >= hi = pure lo
      | otherwise = do
          let mid = (lo + hi) `div` 2
          a <- measureText (T.take mid t)
          b <- measureText (T.take (mid + 1) t)
          if x < (a + b) / 2 then search lo mid else search (mid + 1) hi

-- | Draw the field box, its text (shifted so the caret stays visible), and
-- the blinking caret while focused.
drawField :: FieldMode -> Int -> Rect -> Editor -> Bool -> Bool -> Theme -> ChibiUI model ()
drawField mode k r ed focused reveal th = do
  let EditState t caret _ = editState ed
      readOnly = mode == ReadOnly
      multiline = mode == MultiLine
      ls = if multiline then textLines t else [(0, t)]
      (caretRow, (lineStart, line)) = caretRowLine ls caret
  when (not readOnly) $ do
    fillRectUI r (themeSurface th)
    strokeRectUI r 1 (if focused then themeAccent th else themeBorder th)
  -- An unfocused single line never scrolls, so it needs neither the caret
  -- pen nor the line widths.
  caretPen <- if focused then measureText (T.take (caret - lineStart) line) else pure 0
  oldShift <- storeRead (findSlot fieldFloat 0 (slotKey SlotTextScroll k))
  oldY <- storeRead (findSlot fieldFloat 0 (slotKey SlotTextScrollY k))
  widths <- if focused || multiline then mapM (measureText . snd) ls else pure []
  hov <- hovered r
  wheel <- scrollDelta
  let inset = rectInflate (-fieldInset mode) r
      inner = inset {rectW = max 0 (rectW inset), rectH = max 0 (rectH inset)}
      innerX = rectX inner
      innerW = rectW inner
      wheelX = oldShift + if multiline && hov then v2X wheel * scrollStep else 0
      wheelY = oldY + if multiline && hov then v2Y wheel * scrollStep else 0
      keepVisible pos size extent offset
        | pos < offset = pos
        | pos + size > offset + extent = pos + size - extent
        | otherwise = offset
      shift = clamp 0 (max 0 (maximum (0 : widths) - innerW + 1)) $
        if focused && (reveal || not multiline) then keepVisible caretPen 1 innerW wheelX
        else if multiline then wheelX else 0
      shiftY = if not multiline then 0 else clamp 0 (max 0 (fromIntegral (length ls) * lineHeight - rectH inner)) $
        if focused && reveal then keepVisible (fromIntegral caretRow * lineHeight) lineHeight (rectH inner) wheelY else wheelY
      top = if multiline then rectY inner - shiftY else alignedTextY (themeTextAlign th) inner
      (a, b) = selection ed
  when (shift /= oldShift) $
    storeUpdate (insertSlot fieldFloat (slotKey SlotTextScroll k) shift)
  when (multiline && shiftY /= oldY) $
    storeUpdate (insertSlot fieldFloat (slotKey SlotTextScrollY k) shiftY)
  withClip inner $ do
    forM_ (zip [0 :: Int ..] ls) $ \(lineIndex, (start, text)) -> do
      let y = top + fromIntegral lineIndex * lineHeight
          origin = V2 (innerX - shift) y
          end = start + T.length text
          newlineSelected = multiline && end < T.length t && b > end
      when (y + lineHeight > rectY inner && y < rectY inner + rectH inner) $ do
        drawTextAt origin 0 0 text (themeText th)
        when (focused && a < b && b > start && (a < end || (a == end && newlineSelected))) $ do
          left <- measureText (T.take (max 0 (a - start)) text)
          right <- measureText (T.take (max 0 (b - start)) text)
          let selected = Rect (innerX - shift + left) y (right - left + if newlineSelected then 4 else 0) lineHeight
          fillRectUI selected (themeAccent th)
          withClip selected (drawTextAt origin 0 0 text (themeAccentText th))
    when focused $ do
      t0 <- uiTime
      inp <- getInput
      -- The caret toggles every half second while the window has focus.
      -- Its quad stays in the draw list while blinked off, so a blink
      -- changes one quad's colour and damages only the caret.
      let half = floor (t0 * 2) :: Int
      when (inputWindowFocused inp) (requestFrameAt (fromIntegral (half + 1) / 2))
      fillRectUI
        (Rect (innerX - shift + caretPen) (top + fromIntegral caretRow * lineHeight) 1 lineHeight)
        (if even half then themeText th else colorTransparent)
  when hov (wantCursor UiCursorText)

-- | An image the backend has registered, drawn at @w@ x @h@.
image :: Int -> Float -> Float -> ChibiUI model ()
image img w h = do
  (_, Rect x y rw rh) <- widgetRect (pure (Size w h))
  drawIO $ \a -> emitQuadUV a (texImage img) x y (x + rw) (y + rh) colorWhite 0 0 1 1

-- | A line plot of @values@, scaled to fit its lowest and highest samples;
-- a flat series draws a line across the middle. The plot is at most 220
-- wide and 80 tall; size it with 'nextWidth' and 'nextHeight'.
plotLines :: [Float] -> ChibiUI model ()
plotLines values = do
  (_, r) <- widgetRect (cappedSize 220 80)
  th <- theme
  fillRectUI r (themeSurface th)
  strokeRectUI r 1 (themeBorder th)
  let inner = rectInflate (-2) r
      n = length values
      left = rectX inner
      right = rectX inner + rectW inner
      midY = rectY inner + rectH inner / 2
      points
        | n == 0 = []
        | n == 1 = [V2 left midY, V2 right midY]
        | otherwise =
            let lo = minimum values
                hi = maximum values
                range = hi - lo
                yAt v = rectY inner + rectH inner * (1 - if range > 0 then (v - lo) / range else 0.5)
                xAt i = left + (right - left) * fromIntegral i / fromIntegral (n - 1)
             in [V2 (xAt i) (yAt v) | (i, v) <- zip [0 :: Int ..] values]
  withClip r (drawPolyline (themeAccent th) points)

-- The line as 2px-wide columns, one per pixel column of each segment,
-- spanning what the segment covers there. Every quad stays a rectangle,
-- as clipping and damage need, and a plot costs about a quad per pixel of
-- width, however many samples or however steep.
drawPolyline :: Color -> [V2] -> ChibiUI model ()
drawPolyline col ps = forM_ (zip ps (drop 1 ps)) $ \(V2 ax ay, V2 bx by) ->
  forM_ [floor ax .. max (floor ax) (ceiling bx - 1) :: Int] $ \c -> do
    let yAt x = ay + (by - ay) * (x - ax) / (bx - ax)
        (y0, y1)
          | bx > ax = (yAt (max ax (fromIntegral c)), yAt (min bx (fromIntegral c + 1)))
          | otherwise = (ay, by)
    fillRectUI (Rect (fromIntegral c - 0.5) (min y0 y1 - 1) 2 (abs (y1 - y0) + 2)) col

-- | A basic table: a header row, zebra-striped data rows, hover
-- highlighting, and row selection on click, or with Up and Down while
-- focused. Returns the selected row index, if any. Column widths come from
-- the widest cell in each column, scaled proportionally to fit the
-- available or explicitly assigned width.
table :: [Text] -> [[Text]] -> ChibiUI model (Maybe Int)
table headers rows = do
  th <- theme
  let cellAt c r = if c < length r then r !! c else ""
      cellPadX = 4
      cellPadY = 3
      colCount = length headers
  naturalWidths <-
    forM [0 .. colCount - 1] $ \c -> do
      hw <- measureText (cellAt c headers)
      rws <- mapM (measureText . cellAt c) rows
      pure (maximum (hw + cellPadX * 2 : map (+ cellPadX * 2) rws))
  let totalW = sum naturalWidths
      rowH = lineHeight + cellPadY * 2
      tableH = rowH * fromIntegral (1 + length rows)
  (wid, r) <- widgetRect (cappedSize totalW tableH)
  let selKey = slotKey SlotTableSel (slotOf wid)
      widths = map (\w -> if totalW > 0 then w * rectW r / totalW else 0) naturalWidths
      columns = zip (scanl (+) (rectX r) widths) widths
      drawRow y cells bg = do
        fillRectUI (Rect (rectX r) y (rectW r) rowH) bg
        forM_ (zip columns (cells ++ repeat "")) $ \((x, w), cell) ->
          drawTextIn (Rect (x + cellPadX) (y + cellPadY)
            (max 0 (w - cellPadX * 2)) lineHeight) cell (themeText th)
        fillRectUI (Rect (rectX r) (y + rowH - 1) (rectW r) 1) (themeBorder th)
  ia <- interaction wid r
  inp <- getInput
  stored <- storeRead (findSlot fieldInt 0 selKey)
  -- Rows count from 1 in the store, so 0 is no selection.
  let n = length rows
      hoverI
        | iHovered ia = Just (floor ((v2Y (inputMousePos inp) - rectY r - rowH) / rowH) :: Int)
        | otherwise = Nothing
      current = if 1 <= stored && stored <= n then stored else 0
      step d = if n == 0 then 0 else clamp 1 n (current + d)
      selIdx
        | pressedIn MouseLeft inp, Just h <- hoverI, h >= 0 && h < n = h + 1
        | iFocused ia && pressedIn KeyDown inp = step 1
        | iFocused ia && pressedIn KeyUp inp = step (-1)
        | otherwise = current
  when (selIdx /= current) (storeUpdate (insertSlot fieldInt selKey selIdx))
  withClip r $ do
    drawRow (rectY r) headers (themeSurface th)
    forM_ (zip3 [0 ..] (iterate (+ rowH) (rectY r + rowH)) rows) $ \(i, y, cells) -> do
      let bg | selIdx == i + 1 = themeSurfaceActive th
             | hoverI == Just i = themeRowHover th
             | odd i = themeRowAlt th
             | otherwise = themeWindow th
      drawRow y cells bg
    when (iFocused ia) (strokeRectUI r 1 (themeAccent th))
  pure (if selIdx > 0 then Just (selIdx - 1) else Nothing)

-- | Clip and scroll a body: the region fills the rest of its scope's width
-- and height (the window's, less padding, at top level); 'nextWidth' and
-- 'nextHeight' size it instead. The wheel scrolls every region under the
-- pointer, and a thin scrollbar appears when the body is taller than the
-- region.
scrollColumn :: ChibiUI model a -> ChibiUI model a
scrollColumn body = do
  (wid, r) <- widgetRect ((\ls -> Size (Layout.remainingWidth ls) (Layout.remainingHeight ls)) <$> readLayout)
  th <- theme
  parent <- readLayout
  let k = slotOf wid
      scrollKey = slotKey SlotScrollY k
      barW = 4

  scroll0 <- storeRead (findSlot fieldFloat 0 scrollKey)
  wheel <- scrollDelta
  hov <- hovered r
  extent <- storeRead (findSlot fieldFloat 0 (slotKey SlotScrollExtent k))
  let offset = clamp 0 (max 0 (extent - rectH r))
        (scroll0 + if hov then v2Y wheel * scrollStep else 0)
      viewport = r {rectW = max 0 (rectW r - barW)}
      content = viewport {rectY = rectY r - offset}
  -- Clip to the region and shift the body up by the scroll.
  layoutState (const ((), Layout.beginViewport content parent))
  a <- withClip viewport (scoped body)
  ls1 <- readLayout
  let contentH = sizeH (Layout.contentSize (V2 (rectX content) (rectY content)) ls1)
      maxScroll = max 0 (contentH - rectH r)
      scroll1 = clamp 0 maxScroll offset
  when (scroll1 /= scroll0) $
    storeUpdate (insertSlot fieldFloat scrollKey scroll1)
  when (contentH /= extent) $
    storeUpdate (insertSlot fieldFloat (slotKey SlotScrollExtent k) contentH)
  when (scroll1 /= offset) requestFrame
  -- A scrollbar when the body overflows.
  withClip r $ when (maxScroll > 0) $ do
    let trackR = Rect (rectX r + rectW r - barW) (rectY r) barW (rectH r)
        thumbH = min (rectH r) (max 8 (rectH r * rectH r / contentH))
        thumbY = rectY r + (rectH r - thumbH) * (scroll1 / maxScroll)
    fillRectUI trackR (themeSurface th)
    fillRectUI (Rect (rectX trackR) thumbY barW thumbH) (themeBorder th)
  layoutState (const ((), parent))
  pure a

-- | How far one wheel step scrolls.
scrollStep :: Float
scrollStep = lineHeight * 3

-- | A 1px horizontal rule across the line's width.
separator :: ChibiUI model ()
separator = do
  (_, r) <- widgetRect ((\w -> Size w 1) <$> availWidth)
  th <- theme
  fillRectUI r (themeBorder th)

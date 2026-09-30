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
  , image
  , useImageRgba
  , table
  , scrollColumn
  , separator
  , availWidth
  ) where

import Control.Monad (foldM, forM, forM_, when, void)
import Data.IORef (readIORef, writeIORef)
import Data.Maybe (fromMaybe)
import Data.Dynamic (fromDynamic, toDyn)
import qualified Data.ByteString as BS
import Data.Text (Text)
import qualified Data.Text as T
import Text.Read (readMaybe)
import ChibiUI.Internal.Context (Context (..))
import ChibiUI.Internal.Font (lineHeight)
import ChibiUI.Internal.Id (WidgetId)
import ChibiUI.Internal.Input
import ChibiUI.Internal.Editor
import ChibiUI.Internal.Menu (openContextMenu)
import ChibiUI.Internal.Layout (LayoutState (..))
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
  clicked <- activate wid r
  hov <- hovered r
  act <- isActive wid
  focused <- isFocused wid
  let surface
        | act = themeSurfaceActive th
        | hov = themeSurfaceHover th
        | otherwise = themeSurface th
  fillRectUI r surface
  strokeRectUI r 1 (if focused then themeAccent th else themeBorder th)
  textInRect r t (themeText th)
  pure clicked

-- Buttons and tree headers share pointer capture and keyboard activation.
activate :: WidgetId -> Rect -> ChibiUI model Bool
activate wid r = do
  addFocusable wid
  hov <- hovered r
  pressed <- mousePressed
  released <- mouseReleased
  when (hov && pressed) (void (claimActive wid) >> requestFocus wid)
  act <- isActive wid
  focused <- isFocused wid
  inp <- getInput
  when hov (wantCursor UiCursorPointer)
  pure ((released && act && hov) || (focused && (pressedIn KeyEnter inp || pressedIn KeySpace inp)))

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
  clicked <- activate wid r
  focused <- isFocused wid
  inp <- getInput
  let open | focused && pressedIn KeyLeft inp = False
           | focused && pressedIn KeyRight inp = True
           | clicked = not wasOpen
           | otherwise = wasOpen
  when (open /= wasOpen) $ do
    storeModify (\st -> ((), (if open then insertSlot fieldInt key 1 else deleteSlot fieldInt key) st))
    requestFrame
  th <- theme
  hov <- hovered r
  active <- isActive wid
  withClip r $ do
    when (hov || active) (fillRectUI r (if active then themeSurfaceActive th else themeSurfaceHover th))
    when focused (strokeRectUI r 1 (themeAccent th))
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
textArea value = textField MultiLine measured (sanitizeText True value) (const id)
  where
    measured = do
      width <- availWidth
      pure (Size (min width 320) (lineHeight * 5 + fieldPad * 2 + 2))

-- | A read-only, selectable/copyable line without field chrome.
selectableText :: Text -> ChibiUI model ()
selectableText value = void (textField ReadOnly measured value (const id))
  where
    measured = do
      Size w h <- textSize value
      avail <- availWidth
      pure (Size (min w avail) h)

-- | An integer field. While focused, Up and Down step by 1, or by 10 with
-- Shift. A commit that does not parse reverts to the value passed in.
intInput :: Int -> ChibiUI model Int
intInput = numberInput

-- | A floating-point field, like 'intInput' with a step of 1 (10 with
-- Shift).
floatInput :: Float -> ChibiUI model Float
floatInput = numberInput

numberInput :: (Eq a, Num a, Read a, Show a) => a -> ChibiUI model a
numberInput value = parseNumber value <$>
  textField SingleLine (fieldSize 120) (T.pack (show value)) (stepNumber value)

parseNumber :: Read a => a -> Text -> a
parseNumber fallback = fromMaybe fallback . readMaybe . T.unpack . T.strip

fieldSize :: Float -> ChibiUI model Size
fieldSize width = do
  avail <- availWidth
  pure (Size (min avail width) (lineHeight + fieldPad * 2 + 2))

data FieldMode = SingleLine | MultiLine | ReadOnly deriving (Eq)

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

textField :: FieldMode -> ChibiUI model Size -> Text -> (Input -> Text -> Text) -> ChibiUI model Text
textField mode measure value transform = do
  (wid, r) <- widgetRect measure
  addFocusable wid
  editTextField mode wid r value transform

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
editTextField :: FieldMode -> WidgetId -> Rect -> Text -> (Input -> Text -> Text) -> ChibiUI model Text
editTextField mode wid r value transform = do
  th <- theme
  let readOnly = mode == ReadOnly
      k = slotOf wid
  -- A click on the field takes the keyboard.
  hov <- hovered r
  pressed <- mousePressed
  inp <- getInput
  when (hov && (pressed || pressedIn MouseRight inp)) (requestFocus wid)
  focused <- isFocused wid
  saved <- storeRead (fromMaybe (Inactive (newEditor value)) . (>>= fromDynamic) . lookupSlot fieldDyn k)
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
          storeModify (\st -> ((), insertSlot fieldDyn k (toDyn next) (reset st)))
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
            storeModify (\st -> ((), insertSlot fieldDyn (slotKey SlotTextCommand k) (toDyn cmd) st))
      openContextMenu wid r $
        (if readOnly then [] else [("Undo", not (null (editUndo ed)), queue Undo),
         ("Redo", not (null (editRedo ed)), queue Redo),
         ("Cut", hasSelection, queue Cut)]) ++ [("Copy", hasSelection, queue Copy)] ++
        (if readOnly then [] else [("Paste", True, queue Paste)]) ++
        [("Select all", not (T.null draft), queue SelectAll)]
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
    delta p = (abs (v2X position - v2X p), abs (v2Y position - v2Y p))
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
  let readOnly = mode == ReadOnly
      multiline = mode == MultiLine
      k = slotOf wid
      pressed = hov && pressedIn MouseLeft inp
      t = editText (editState ed0)
  when pressed (void (claimActive wid))
  active <- isActive wid
  offset <- storeRead (findSlot fieldFloat 0 (slotKey SlotTextScroll k))
  offsetY <- storeRead (findSlot fieldFloat 0 (slotKey SlotTextScrollY k))
  previous <- storeRead ((>>= fromDynamic) . lookupSlot fieldDyn (slotKey SlotTextClick k))
  now <- uiTime
  when (active && heldIn MouseLeft inp && not (rectHit r (inputMousePos inp))) requestFrame
  ed1 <- case stepPointer inp hov active now previous of
    KeepSelection -> pure ed0
    gesture -> do
      let ls = textLines t
          lineIndex = max 0 (min (length ls - 1)
            (floor ((v2Y (inputMousePos inp) - rectY r - fieldPad + offsetY) / lineHeight)))
          (start, line) = if multiline then ls !! lineIndex else (0, t)
      caret <- (start +) <$> hitCaret line (v2X (inputMousePos inp) - rectX r - (if readOnly then 0 else fieldPad) + offset)
      let anchor = if pressed && not (modShift (inputModifiers inp)) then caret else editAnchor (editState ed0)
          pointed = select anchor caret ed0
      case gesture of
        ClickSelection click@(ClickState _ _ clicks) -> do
          storeModify (\st -> ((), insertSlot fieldDyn (slotKey SlotTextClick k) (toDyn click) st))
          pure $ case clicks of
            2 -> selectWord caret pointed
            3 | multiline -> select start (start + T.length line) pointed
              | otherwise -> command SelectAll pointed
            _ -> pointed
        _ -> pure pointed
  pending <- storeRead ((>>= fromDynamic) . lookupSlot fieldDyn (slotKey SlotTextCommand k))
  storeModify (\st -> ((), deleteSlot fieldDyn (slotKey SlotTextCommand k) st))
  let allowed cmd = not readOnly || case cmd of
        Move _ _ -> True
        SelectAll -> True
        Copy -> True
        _ -> False
  foldM (runEdit multiline) ed1 (filter allowed (maybe [] (: []) pending ++ inputCommands multiline inp))

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
      pad = if readOnly then 0 else fieldPad
      ls = if multiline then textLines t else [(0, t)]
      (lineStart, line) = if multiline then caretLine t caret else (0, t)
      caretRow = if multiline then T.count "\n" (T.take caret t) else 0
  when (not readOnly) $ do
    fillRectUI r (themeSurface th)
    strokeRectUI r 1 (if focused then themeAccent th else themeBorder th)
  caretPen <- measureText (T.take (caret - lineStart) line)
  oldShift <- storeRead (findSlot fieldFloat 0 (slotKey SlotTextScroll k))
  oldY <- storeRead (findSlot fieldFloat 0 (slotKey SlotTextScrollY k))
  widths <- mapM (measureText . snd) ls
  hov <- hovered r
  wheel <- scrollDelta
  let innerX = rectX r + pad
      innerW = max 0 (rectW r - pad * 2)
      inner = Rect innerX (rectY r + pad) innerW (max 0 (rectH r - pad * 2))
      wheelX = oldShift + if multiline && hov then v2X wheel * lineHeight * 3 else 0
      wheelY = oldY + if multiline && hov then v2Y wheel * lineHeight * 3 else 0
      keepVisible pos size extent offset
        | pos < offset = pos
        | pos + size > offset + extent = pos + size - extent
        | otherwise = offset
      shift = clamp 0 (max 0 (maximum widths - innerW + 1)) $
        if focused && (reveal || not multiline) then keepVisible caretPen 1 innerW wheelX
        else if multiline then wheelX else 0
      shiftY = if not multiline then 0 else clamp 0 (max 0 (fromIntegral (length ls) * lineHeight - rectH inner)) $
        if focused && reveal then keepVisible (fromIntegral caretRow * lineHeight) lineHeight (rectH inner) wheelY else wheelY
      top = if multiline then rectY inner - shiftY else alignedTextY (themeTextAlign th) inner
      (a, b) = selection ed
  when (shift /= oldShift) $
    storeModify (\st -> ((), insertSlot fieldFloat (slotKey SlotTextScroll k) shift st))
  when (multiline && shiftY /= oldY) $
    storeModify (\st -> ((), insertSlot fieldFloat (slotKey SlotTextScrollY k) shiftY st))
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
      let blink = floor (t0 * 2) `mod` (2 :: Int) == (0 :: Int)
      when blink $
        fillRectUI
          (Rect (innerX - shift + caretPen) (top + fromIntegral caretRow * lineHeight) 1 lineHeight)
          (themeText th)
  when hov (wantCursor UiCursorText)

-- | An image the backend has registered, drawn at @w@ x @h@.
image :: Int -> Float -> Float -> ChibiUI model ()
image img w h = do
  (_, r) <- widgetRect (pure (Size w h))
  drawImageUV img r

-- | Register an RGBA image under an id: @ieWidth@ x @ieHeight@ pixels, top
-- row first, four bytes per pixel. Call it every frame the image shows;
-- the backend uploads it when @version@ changes. Draw it with 'image'.
useImageRgba :: Int -> Int -> Int -> Int -> BS.ByteString -> ChibiUI model ()
useImageRgba = registerImage

-- | A basic table: a header row, zebra-striped data rows, hover
-- highlighting, and row selection on click. Returns the selected row
-- index, if any. Column widths come from the widest cell in each column,
-- scaled proportionally to fit the available or explicitly assigned width.
table :: [Text] -> [[Text]] -> ChibiUI model (Maybe Int)
table headers rows = do
  wid <- nextId
  th <- theme
  let k = slotOf wid
      selKey = slotKey SlotTableSel k
      cellAt c r = if c < length r then r !! c else ""
      cellPadX = 4
      cellPadY = 3
      colCount = length headers
  naturalWidths <-
    forM [0 .. colCount - 1] $ \c -> do
      hw <- measureText (cellAt c headers)
      rws <- mapM (measureText . cellAt c) rows
      pure (maximum (hw + cellPadX * 2 : map (+ cellPadX * 2) rws))
  avail <- availWidth
  let totalW = sum naturalWidths
      rowH = lineHeight + cellPadY * 2
      tableH = rowH * fromIntegral (1 + length rows)
  r <- place (Size (min avail totalW) tableH)
  recordRect wid r
  let widths = map (\w -> if totalW > 0 then w * rectW r / totalW else 0) naturalWidths
      columns = zip (scanl (+) (rectX r) widths) widths
      drawRow y cells bg = do
        fillRectUI (Rect (rectX r) y (rectW r) rowH) bg
        forM_ (zip columns (cells ++ repeat "")) $ \((x, w), cell) ->
          drawTextIn (Rect (x + cellPadX) (y + cellPadY)
            (max 0 (w - cellPadX * 2)) lineHeight) cell (themeText th)
        fillRectUI (Rect (rectX r) (y + rowH - 1) (rectW r) 1) (themeBorder th)
  mouse <- mousePos
  pressed <- mousePressed
  inTable <- hovered r
  let hoverI
        | inTable = Just (floor ((v2Y mouse - rectY r - rowH) / rowH) :: Int)
        | otherwise = Nothing
      hoverValid = maybe False (\i -> i >= 0 && i < length rows) hoverI
  when (pressed && hoverValid) $
    storeModify (\st -> ((), insertSlot fieldInt selKey (fromMaybe 0 hoverI + 1) st))
  sel1 <- storeRead (findSlot fieldInt 0 selKey)
  let selIdx = if 1 <= sel1 && sel1 <= length rows then sel1 else 0
  withClip r $ do
    drawRow (rectY r) headers (themeSurface th)
    forM_ (zip3 [0 ..] (iterate (+ rowH) (rectY r + rowH)) rows) $ \(i, y, cells) -> do
      let bg | selIdx == i + 1 = themeSurfaceActive th
             | hoverI == Just i = themeRowHover th
             | odd i = themeRowAlt th
             | otherwise = themeWindow th
      drawRow y cells bg
  when inTable (wantCursor UiCursorPointer)
  pure (if selIdx > 0 then Just (selIdx - 1) else Nothing)

-- | Clip and scroll a body: the region fills the line's width and runs to
-- the window's bottom padding. The wheel scrolls it while the pointer is
-- over it, and a thin scrollbar appears when the body is taller than the
-- region. Bodies do not nest.
scrollColumn :: ChibiUI model a -> ChibiUI model a
scrollColumn body = do
  wid <- nextId
  th <- theme
  winH <- sizeH <$> windowSize
  let k = slotOf wid
      scrollKey = slotKey SlotScrollY k
      barW = 4
  ls0 <- readLayout
  avail <- availWidth
  let top = lsLineY ls0
      regionH = max 0 (winH - themeWindowPad th - top)
  r <- place (Size avail regionH)
  parent <- readLayout
  recordRect wid r
  scroll0 <- storeRead (findSlot fieldFloat 0 scrollKey)
  wheel <- scrollDelta
  hov <- hovered r
  extent <- storeRead (findSlot fieldFloat 0 (slotKey SlotScrollExtent k))
  let offset = clamp 0 (max 0 (extent - rectH r))
        (scroll0 + if hov then v2Y wheel * scrollStep else 0)
      viewport = r {rectW = max 0 (rectW r - barW)}
      content = viewport {rectY = rectY r - offset}
  ctx <- askContext
  -- Clip to the region and shift the body up by the scroll.
  liftIO (writeIORef (ctxLayout ctx) (Layout.beginViewport content parent))
  a <- withClip viewport (scoped body)
  ls1 <- readLayout
  let contentH = sizeH (Layout.contentSize (V2 (rectX content) (rectY content)) ls1)
      maxScroll = max 0 (contentH - rectH r)
      scroll1 = clamp 0 maxScroll offset
  when (scroll1 /= scroll0) $
    storeModify (\st -> ((), insertSlot fieldFloat scrollKey scroll1 st))
  when (contentH /= extent) $
    storeModify (\st -> ((), insertSlot fieldFloat (slotKey SlotScrollExtent k) contentH st))
  when (scroll1 /= offset) requestFrame
  -- A scrollbar when the body overflows.
  withClip r $ when (maxScroll > 0) $ do
    let trackR = Rect (rectX r + rectW r - barW) (rectY r) barW (rectH r)
        thumbH = min (rectH r) (max 8 (rectH r * rectH r / contentH))
        thumbY = rectY r + (rectH r - thumbH) * (scroll1 / maxScroll)
    fillRectUI trackR (themeSurface th)
    fillRectUI (Rect (rectX trackR) thumbY barW thumbH) (themeBorder th)
  liftIO (writeIORef (ctxLayout ctx) parent)
  pure a
  where
    scrollStep = lineHeight * 3

-- | A 1px horizontal rule across the line's width.
separator :: ChibiUI model ()
separator = do
  (_, r) <- widgetRect ((\w -> Size w 1) <$> availWidth)
  th <- theme
  fillRectUI r (themeBorder th)

-- | The width a widget filling the line can take.
availWidth :: ChibiUI model Float
availWidth = availableWidth

readLayout :: ChibiUI model LayoutState
readLayout = do
  ctx <- askContext
  liftIO (readIORef (ctxLayout ctx))

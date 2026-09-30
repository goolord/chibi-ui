-- | The widgets. Each is an ordinary call: take an id, place a rect at the
-- cursor, read this frame's input, draw, return what the user did.
module ChibiUI.Internal.Widgets
  ( label
  , labelDim
  , selectableText
  , button
  , checkbox
  , radio
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

import Control.Monad (foldM, forM, forM_, mfilter, msum, when, void)
import Data.List (transpose)
import Data.Maybe (fromMaybe, isJust)
import Data.Text (Text)
import qualified Data.Text as T
import Text.Read (readMaybe)
import ChibiUI.Internal.Draw (emitQuadUV, fillRect, texImage)
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
  (_, r, i) <- interactive clickable $ do
    Size w h <- textSize t
    pure (Size (w + widgetPad * 2) (h + widgetPad * 2))
  th <- theme
  fillRectUI r (surfaceFor th i)
  frameBorder th r (iFocused i)
  textInRect r t (themeText th)
  pure (iClicked i)

-- | A box with a caption beside it. Click it, or press Enter/Space while
-- it is focused, to flip it; returns the value it now holds.
checkbox :: Text -> Bool -> ChibiUI model Bool
checkbox t value = do
  let flipped clicked = clicked /= value
  flipped <$> markBox t flipped

-- | One box per option, in a row: click one, or press Enter/Space while it
-- is focused, to choose it. Returns the value now chosen.
radio :: Eq a => [(Text, a)] -> a -> ChibiUI model a
radio options value = row $ do
  clicks <- forM options $ \(t, x) -> do
    clicked <- markBox t (const (x == value))
    pure (if clicked then Just x else Nothing)
  case msum clicks of
    -- Options before the chosen one drew already: show them next frame.
    Just x | x /= value -> x <$ requestFrame
    _ -> pure value

-- | A box with a caption after it, filled while @marked@ says, given
-- whether it was clicked this frame. 'True' on click, or Enter/Space
-- while focused.
markBox :: Text -> (Bool -> Bool) -> ChibiUI model Bool
markBox t marked = do
  (_, r, i) <- interactive clickable $ do
    Size w h <- textSize t
    pure (Size (h + widgetPad + w) (h + widgetPad * 2))
  th <- theme
  let box = Rect (rectX r) (rectY r + widgetPad) lineHeight lineHeight
      caption = lineHeight + widgetPad
  fillRectUI box (surfaceFor th i)
  frameBorder th box (iFocused i)
  when (marked (iClicked i)) (fillRectUI (rectInflate (-4) box) (themeAccent th))
  drawTextIn (r {rectX = rectX r + caption, rectW = max 0 (rectW r - caption)}) t (themeText th)
  pure (iClicked i)

-- | The surface under a widget: pressed, hovered, or at rest.
surfaceFor :: Theme -> Interaction -> Color
surfaceFor th i
  | iActive i = themeSurfaceActive th
  | iHovered i = themeSurfaceHover th
  | otherwise = themeSurface th

-- | A widget's 1px border: the accent while focused.
frameBorder :: Theme -> Rect -> Bool -> ChibiUI model ()
frameBorder th r focused = strokeRectUI r 1 (if focused then themeAccent th else themeBorder th)

-- | A 1px accent border while focused, for widgets with no border at rest.
focusRing :: Theme -> Rect -> Bool -> ChibiUI model ()
focusRing th r focused = when focused (strokeRectUI r 1 (themeAccent th))

-- | What the pointer and keyboard did to a widget this frame, and the
-- frame's input, for the rest of what the widget reads.
data Interaction = Interaction
  { iHovered, iActive, iFocused, iClicked :: !Bool
  , iInput :: !Input
  }

-- | The value for the first of these keys pressed while the widget has
-- the keyboard: a widget's own small key table.
focusedKey :: Interaction -> [(Key, a)] -> Maybe a
focusedKey i keys = if iFocused i then firstPressed (iInput i) keys else Nothing

-- | How a widget takes the pointer and the keyboard.
data InteractionSpec = InteractionSpec
  { pressTyping :: !Bool
  -- ^ Whether it takes typing, so app keys stand down while it is focused.
  , pressCursor :: !UiCursorKind
  , pressRightFocus :: !Bool
  -- ^ Whether a right press takes the keyboard too, ahead of its menu.
  }

-- | Buttons, tree headers, sliders and tables.
clickable :: InteractionSpec
clickable = InteractionSpec False UiCursorPointer False

-- | Text fields.
editable :: InteractionSpec
editable = InteractionSpec True UiCursorText True

-- Every widget that takes input shares Tab reachability, the pointer grab
-- a press takes (with the keyboard), and its cursor while hovered or
-- grabbed. A click is a release over the widget the press grabbed, or
-- Enter/Space while focused.
interaction :: InteractionSpec -> WidgetId -> Rect -> ChibiUI model Interaction
interaction p wid r = do
  addFocusable wid r (pressTyping p)
  hov <- hovered r
  inp <- getInput
  let press = hov && pressedIn MouseLeft inp
  when press (void (claimActive wid))
  when (press || (hov && pressRightFocus p && pressedIn MouseRight inp)) (requestFocus wid)
  act <- isActive wid
  focused <- isFocused wid
  when (hov || act) (wantCursor (pressCursor p))
  pure Interaction
    { iHovered = hov
    , iActive = act
    , iFocused = focused
    , iClicked = (releasedIn MouseLeft inp && act && hov) || (focused && any (`pressedIn` inp) [KeyEnter, KeySpace])
    , iInput = inp
    }

-- | Place an input-taking widget: its id, its rect, and what it got.
interactive :: InteractionSpec -> ChibiUI model Size -> ChibiUI model (WidgetId, Rect, Interaction)
interactive p measure = do
  (wid, r) <- widgetRect measure
  i <- interaction p wid r
  pure (wid, r, i)

-- | A collapsible branch, initially closed. Click or Enter/Space toggles;
-- Left closes and Right opens a focused header. Children run only while open.
-- Nest nodes freely; use 'withKey' when siblings can reorder. The branch is
-- one layout group, so opening it never changes the identity of later siblings.
treeNode :: Text -> ChibiUI model () -> ChibiUI model ()
treeNode title body = column $ do
  (wid, r, i) <- interactive clickable $ do
    width <- availWidth
    pure (Size width (lineHeight + widgetPad * 2))
  let gutter = lineHeight + widgetPad
  wasOpen <- isJust <$> widgetState treeOpen wid
  let open = fromMaybe (if iClicked i then not wasOpen else wasOpen)
        (focusedKey i [(KeyLeft, False), (KeyRight, True)])
  when (open /= wasOpen) $ do
    setWidgetState treeOpen wid (if open then Just () else Nothing)
    requestFrame
  th <- theme
  withClip r $ do
    when (iHovered i || iActive i) (fillRectUI r (surfaceFor th i))
    focusRing th r (iFocused i)
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
  (_, r, i) <- interactive clickable (fieldSize 160)
  th <- theme
  let inp = iInput i
      range = hi - lo
      frac v = if range > 0 then clamp01 ((v - lo) / range) else 0
      atFrac f = lo + f * range
      underPointer = clamp01 ((v2X (inputMousePos inp) - rectX r) / max 1 (rectW r))
      step = range / 10
      moved
        | iActive i && heldIn MouseLeft inp = atFrac underPointer
        | Just d <- focusedKey i [(KeyLeft, -step), (KeyRight, step)] = clamp lo hi (value + d)
        | otherwise = value
      tw = min 8 (max 0 (rectW r))
      thumbX = rectX r + tw / 2 + frac moved * max 0 (rectW r - tw)
      cy = rectY r + rectH r / 2
      thumbH = min 14 (rectH r)
  fillRectUI (Rect (rectX r) (cy - 2) (rectW r) 4) (themeSurface th)
  fillRectUI (Rect (rectX r) (cy - 2) (max 0 (thumbX - rectX r)) 4) (themeAccent th)
  fillRectUI (Rect (thumbX - tw / 2) (cy - thumbH / 2) tw thumbH) (themeText th)
  focusRing th r (iFocused i)
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

data DraftEvent = BeginEdit !Text | UpdateEdit !Editor | CommitEdit | CancelEdit

draftEditor :: Draft -> Editor
draftEditor = \case
  Inactive ed -> ed
  Editing _ ed -> ed

editing :: Draft -> Bool
editing = \case
  Editing _ _ -> True
  Inactive _ -> False

stepDraft :: DraftEvent -> Draft -> Draft
stepDraft event draft = case (event, draft) of
  (BeginEdit value, Inactive ed) -> Editing value
    (if editorText ed == value then ed else newEditor value)
  (UpdateEdit ed, Editing original _) -> Editing original ed
  (UpdateEdit ed, Inactive _) -> Inactive ed
  (CommitEdit, _) -> Inactive (draftEditor draft)
  (CancelEdit, Editing original _) -> Inactive (newEditor original)
  _ -> draft

-- Stepping updates the same draft that is painted and subsequently edited.
stepNumber :: (Eq a, Num a, Read a, Show a) => a -> Input -> Text -> Text
stepNumber value inp t
  | delta == 0 = t
  | otherwise = T.pack (show (parseNumber value t + delta))
  where
    amount = if modShift (inputModifiers inp) then 10 else 1
    delta = fromMaybe 0 (firstPressed inp [(KeyUp, amount), (KeyDown, negate amount)])

-- | The shared editing lifecycle: seeding the draft on focus, folding this
-- frame's keys and text into it, scrolling to the caret, drawing, and the
-- value the caller should keep.
textField :: FieldMode -> ChibiUI model Size -> Text -> (Input -> Text -> Text) -> ChibiUI model Text
textField mode measure value transform = do
  (wid, r, i) <- interactive editable measure
  saved <- fromMaybe (FieldState (Inactive (newEditor value)) (V2 0 0) Nothing Nothing)
    <$> widgetState textFields wid
  let inp = iInput i
      readOnly = mode == ReadOnly
      focused = iFocused i
      escaped = focused && pressedIn KeyEscape inp
      before = fieldDraft saved
      started = if focused then stepDraft (BeginEdit value) before else before
      current = if readOnly && editorText (draftEditor started) /= value
        then stepDraft (UpdateEdit (newEditor value)) started else started
      -- The next state around a draft; the scroll resets when an edit
      -- ends. Only a draft that stopped editing compares: an editing
      -- draft's equality walks its whole undo history.
      settle draft = saved
        { fieldDraft = draft
        , fieldScroll = if not (editing draft) && draft /= before then V2 0 0 else fieldScroll saved
        }
      -- A draft the frame ended or cancelled, without editing it.
      settled draft = pure (Just (settle draft), draftEditor draft, False)
  -- This frame's state (none for an idle field), the editor to show, and
  -- whether the caret moved by editing, so the view scrolls to it.
  (next, shown, reveal) <- case (focused, current) of
    (False, Inactive _) -> pure (Nothing, newEditor value, False)
    (False, Editing _ _) -> settled (stepDraft CommitEdit current)
    _ | escaped -> settled (if readOnly then Inactive (newEditor value) else stepDraft CancelEdit current)
    _ -> do
      (edited, click) <- editStep mode i r saved (draftEditor current)
      let text = transform inp (editorText edited)
          ed = if text == editorText edited then edited else replace text (command SelectAll edited)
          updated = stepDraft (UpdateEdit ed) current
          d = if mode /= MultiLine && pressedIn KeyEnter inp then stepDraft CommitEdit updated else updated
          moved = not (editing before) || editState ed /= editState (draftEditor current)
            || (iHovered i && pressedIn MouseLeft inp) || not (null (inputKeys inp))
      pure (Just (settle d) {fieldClick = click, fieldQueued = Nothing}, ed, moved)
  let st = fromMaybe saved next
      active = editing (fieldDraft st)
  layout@(FieldLayout _ _ _ _ scroll) <- fieldLayout mode r shown active reveal (fieldScroll st)
  let scrolled = scroll /= fieldScroll st
      final = if scrolled then st {fieldScroll = scroll} else st
  -- An unchanged state keeps its entry: storing an equal value rebuilds
  -- the store's map for nothing.
  when (scrolled || maybe False (/= saved) next) (setWidgetState textFields wid (Just final))
  when (focused && not active) blurFocus
  th <- theme
  drawField mode r shown active layout th
  when (focused && not escaped) $ do
    -- A focused field always has an entry. The action keeps only the id
    -- and the command, not this frame's state, while the menu is open.
    let hasSelection = uncurry (/=) (selection shown)
        queue cmd = do
          requestFocus wid
          latest <- widgetState textFields wid
          forM_ latest $ \st' -> setWidgetState textFields wid (Just st' {fieldQueued = Just cmd})
    openContextMenu wid r
      [ (title, enabled, queue cmd)
      | (title, enabled, cmd) <-
          [ ("Undo", not (null (editUndo shown)), Undo)
          , ("Redo", not (null (editRedo shown)), Redo)
          , ("Cut", hasSelection, Cut)
          , ("Copy", hasSelection, Copy)
          , ("Paste", True, Paste)
          , ("Select all", not (T.null (editorText shown)), SelectAll)
          ]
      , commandAllowed mode cmd
      ]
  pure (editorText shown)

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
-- Hit tests use the scroll the text was last drawn at. Returns the edited
-- editor and the last press.
editStep :: FieldMode -> Interaction -> Rect -> FieldState -> Editor -> ChibiUI model (Editor, Maybe ClickState)
editStep mode i r saved ed0 = do
  let inp = iInput i
      multiline = mode == MultiLine
      hov = iHovered i
      active = iActive i
      pressed = hov && pressedIn MouseLeft inp
      t = editorText ed0
      inner = fieldInner mode r
      V2 offset offsetY = fieldScroll saved
      V2 px py = inputMousePos inp
  now <- uiTime
  when (active && heldIn MouseLeft inp && not (rectHit r (inputMousePos inp))) requestFrame
  (ed1, click) <- case stepPointer inp hov active now (fieldClick saved) of
    KeepSelection -> pure (ed0, fieldClick saved)
    gesture -> do
      let ls = textLines t
          lineIndex = clamp 0 (length ls - 1) (floor ((py - rectY inner + offsetY) / lineHeight))
          (start, line) = if multiline then ls !! lineIndex else (0, t)
      caret <- (start +) <$> hitCaret line (px - rectX inner + offset)
      let anchor = if pressed && not (modShift (inputModifiers inp)) then caret else editAnchor (editState ed0)
          pointed = select anchor caret ed0
      pure $ case gesture of
        ClickSelection press@(ClickState _ _ clicks) ->
          ( case clicks of
              2 -> selectWord caret pointed
              3 | multiline -> select start (start + T.length line) pointed
                | otherwise -> command SelectAll pointed
              _ -> pointed
          , Just press
          )
        _ -> (pointed, fieldClick saved)
  let commands = maybe id (:) (fieldQueued saved) (inputCommands multiline inp)
  ed <- foldM (runEdit multiline) ed1 (filter (commandAllowed mode) commands)
  pure (ed, click)

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

-- | The box a field's text scrolls within.
fieldInner :: FieldMode -> Rect -> Rect
fieldInner mode r =
  let inset = rectInflate (-fieldInset mode) r
   in inset {rectW = max 0 (rectW inset), rectH = max 0 (rectH inset)}

-- | Where a field's text sits this frame, worked out once for drawing: the
-- box it scrolls within, its lines, the caret's row and its offset along
-- that line (measured only while editing), and the scroll.
data FieldLayout = FieldLayout !Rect [(Int, Text)] !Int !Float !V2

-- | Lay out a field's text: the wheel scrolls a hovered text area, and
-- while editing the caret stays in view, on every frame for a single line
-- and after an edit (@reveal@) for a text area.
fieldLayout :: FieldMode -> Rect -> Editor -> Bool -> Bool -> V2 -> ChibiUI model FieldLayout
fieldLayout mode r ed focused reveal old = do
  let EditState t caret _ = editState ed
      multiline = mode == MultiLine
      inner = fieldInner mode r
      ls = if multiline then textLines t else [(0, t)]
      (caretRow, (lineStart, line)) = caretRowLine ls caret
  -- An unfocused single line never scrolls, so it needs no caret pen.
  caretPen <- if focused then measureText (T.take (caret - lineStart) line) else pure 0
  V2 wheelX wheelY <- if multiline then wheelScroll r old else pure old
  let keepVisible pos size extent offset
        | pos < offset = pos
        | pos + size > offset + extent = pos + size - extent
        | otherwise = offset
      wantX
        | focused && (reveal || not multiline) = keepVisible caretPen 1 (rectW inner) wheelX
        | multiline = wheelX
        | otherwise = 0
  -- Only a scroll right of the start needs the widest line to clamp it.
  x <- if wantX <= 0 then pure 0 else do
    widths <- mapM (measureText . snd) ls
    pure (clampScroll (maximum (0 : widths) + 1) (rectW inner) wantX)
  let y
        | not multiline = 0
        | otherwise = clampScroll (fromIntegral (length ls) * lineHeight) (rectH inner) $
            if focused && reveal
              then keepVisible (fromIntegral caretRow * lineHeight) lineHeight (rectH inner) wheelY
              else wheelY
  pure (FieldLayout inner ls caretRow caretPen (V2 x y))

-- | Draw the field box, its text at the layout's scroll, and the blinking
-- caret while focused.
drawField :: FieldMode -> Rect -> Editor -> Bool -> FieldLayout -> Theme -> ChibiUI model ()
drawField mode r ed focused (FieldLayout inner ls caretRow caretPen (V2 shift shiftY)) th = do
  let t = editorText ed
      multiline = mode == MultiLine
      innerX = rectX inner
      top = if multiline then rectY inner - shiftY else alignedTextY (themeTextAlign th) inner
      (a, b) = selection ed
  when (mode /= ReadOnly) $ do
    fillRectUI r (themeSurface th)
    frameBorder th r focused
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

-- The line through points in ascending x, as 2px-wide columns, one per
-- pixel column, spanning what every segment covers there. Every quad stays
-- a rectangle, as clipping and damage need, and a plot costs at most a
-- quad per pixel of width, however many samples or however steep: the
-- segments crossing one column arrive together and merge into one span.
drawPolyline :: Color -> [V2] -> ChibiUI model ()
drawPolyline col ps = drawIO $ \arena ->
  let
    -- Column @k@ of the segment from @(ax, ay)@ to @(bx, by)@; the open
    -- column @c@ spans @lo .. hi@, 'minBound' while none is open. A
    -- segment's last column is often the next one's first, which then
    -- merges into it.
    walk !c !lo !hi !k !ax !ay !bx !by more
      | k > max (floor ax) (ceiling bx - 1) = case more of
          V2 nx ny : more' -> walk c lo hi (floor bx) bx by nx ny more'
          [] -> emit c lo hi
      | otherwise =
          let yAt x = ay + (by - ay) * (x - ax) / (bx - ax)
              !y0 = if bx > ax then yAt (max ax (fromIntegral k)) else ay
              !y1 = if bx > ax then yAt (min bx (fromIntegral k + 1)) else by
              !a = min y0 y1
              !b = max y0 y1
           in if k == c
                then walk c (min lo a) (max hi b) (k + 1) ax ay bx by more
                else emit c lo hi >> walk k a b (k + 1) ax ay bx by more
    emit !c !lo !hi =
      when (c /= minBound) (fillRect arena (Rect (fromIntegral c - 0.5) (lo - 1) 2 (hi - lo + 2)) col)
   in case ps of
        V2 x0 y0 : V2 x1 y1 : more -> walk (minBound :: Int) 0 0 (floor x0) x0 y0 x1 y1 more
        _ -> pure ()

-- | A basic table: a header row, zebra-striped data rows, hover
-- highlighting, and row selection on click, or with Up and Down while
-- focused. Returns the selected row index, if any. Column widths come from
-- the widest cell in each column, scaled proportionally to fit the
-- available or explicitly assigned width.
table :: [Text] -> [[Text]] -> ChibiUI model (Maybe Int)
table headers rows = do
  th <- theme
  let cellPadX = 4
      cellPadY = 3
      colCount = length headers
      -- One cell per header in every row: ragged rows pad with blanks.
      cells = map (take colCount . (++ repeat "")) rows
  naturalWidths <- forM (transpose (headers : cells)) $ \col ->
    (+ cellPadX * 2) . maximum <$> mapM measureText col
  let totalW = sum naturalWidths
      rowH = lineHeight + cellPadY * 2
      tableH = rowH * fromIntegral (1 + length rows)
  (wid, r, ia) <- interactive clickable (cappedSize totalW tableH)
  let widths = map (\w -> if totalW > 0 then w * rectW r / totalW else 0) naturalWidths
      columns = zip (scanl (+) (rectX r) widths) widths
      drawRow y line bg = do
        fillRectUI (Rect (rectX r) y (rectW r) rowH) bg
        forM_ (zip columns line) $ \((x, w), cell) ->
          drawTextIn (Rect (x + cellPadX) (y + cellPadY)
            (max 0 (w - cellPadX * 2)) lineHeight) cell (themeText th)
        fillRectUI (Rect (rectX r) (y + rowH - 1) (rectW r) 1) (themeBorder th)
  stored <- widgetState tableSelection wid
  let inp = iInput ia
      n = length rows
      hoverI
        | iHovered ia = Just (floor ((v2Y (inputMousePos inp) - rectY r - rowH) / rowH) :: Int)
        | otherwise = Nothing
      current = mfilter (\k -> 0 <= k && k < n) stored
      -- Down from no selection takes the first row, as does Up.
      step d = if n == 0 then Nothing else Just (clamp 0 (n - 1) (fromMaybe (-1) current + d))
      selected
        | pressedIn MouseLeft inp, Just h <- hoverI, h >= 0 && h < n = Just h
        | Just d <- focusedKey ia [(KeyDown, 1), (KeyUp, -1)] = step d
        | otherwise = current
  when (selected /= stored) (setWidgetState tableSelection wid selected)
  withClip r $ do
    drawRow (rectY r) headers (themeSurface th)
    forM_ (zip3 [0 ..] (iterate (+ rowH) (rectY r + rowH)) cells) $ \(i, y, line) -> do
      let bg | selected == Just i = themeSurfaceActive th
             | hoverI == Just i = themeRowHover th
             | odd i = themeRowAlt th
             | otherwise = themeWindow th
      drawRow y line bg
    focusRing th r (iFocused ia)
  pure selected

-- | Clip and scroll a body: the region fills the rest of its scope's width
-- and height (the window's, less padding, at top level); 'nextWidth' and
-- 'nextHeight' size it instead. The wheel scrolls every region under the
-- pointer, and a thin scrollbar appears when the body is taller than the
-- region. Drag the scrollbar's thumb to scroll, or press the track to bring
-- the thumb under the pointer and drag from there.
scrollColumn :: ChibiUI model a -> ChibiUI model a
scrollColumn body = do
  (wid, r) <- widgetRect ((\ls -> Size (Layout.remainingWidth ls) (Layout.remainingHeight ls)) <$> readLayout)
  th <- theme
  let barW = 4
      trackR = Rect (rectX r + rectW r - barW) (rectY r) barW (rectH r)
  -- The extent is last frame's: the body has not run yet.
  saved@(ScrollState scroll0 extent grab0) <- fromMaybe (ScrollState 0 0 0) <$> widgetState scrollRegions wid
  wheeled <- clampScroll extent (rectH r) . v2Y <$> wheelScroll r (V2 0 scroll0)
  -- The scrollbar takes the pointer where it showed last frame.
  barHov <- if extent > rectH r then hovered trackR else pure False
  inp <- getInput
  let py = v2Y (inputMousePos inp)
      (thumbY0, thumbH0) = scrollThumb r extent wheeled
      pressed = barHov && pressedIn MouseLeft inp
  when pressed (void (claimActive wid))
  dragging <- (&& heldIn MouseLeft inp) <$> isActive wid
  -- A press on the thumb holds it where it was pressed; one on the track
  -- holds it by its middle, so the thumb jumps under the pointer.
  let grab
        | pressed = if py >= thumbY0 && py < thumbY0 + thumbH0 then py - thumbY0 else thumbH0 / 2
        | otherwise = grab0
      travel = rectH r - thumbH0
      offset
        | dragging && travel > 0 =
            clampScroll extent (rectH r) ((py - grab - rectY r) / travel * (extent - rectH r))
        | otherwise = wheeled
  let viewport = r {rectW = max 0 (rectW r - barW)}
      content = viewport {rectY = rectY r - offset}
  -- Clip to the region and shift the body up by the scroll.
  (a, Size _ contentH) <- layoutScope (const (Layout.beginViewport content))
    (\parent inner -> (Layout.contentSize (V2 (rectX content) (rectY content)) inner, parent))
    (withClip viewport body)
  let maxScroll = max 0 (contentH - rectH r)
      scroll1 = clampScroll contentH (rectH r) offset
      next = ScrollState scroll1 contentH grab
  when (next /= saved) (setWidgetState scrollRegions wid (Just next))
  -- A shrunk body moved the clamp: settle the new offset on screen.
  when (scroll1 /= offset) requestFrame
  -- A scrollbar when the body overflows, brighter while hovered or dragged.
  withClip r $ when (maxScroll > 0) $ do
    let (thumbY, thumbH) = scrollThumb r contentH scroll1
    fillRectUI trackR (themeSurface th)
    fillRectUI (Rect (rectX trackR) thumbY barW thumbH)
      (if barHov || dragging then themeTextDim th else themeBorder th)
  pure a

-- | The top and height of a region's scrollbar thumb for a body @content@
-- tall, scrolled to @offset@.
scrollThumb :: Rect -> Float -> Float -> (Float, Float)
scrollThumb r content offset =
  let view = rectH r
      maxScroll = content - view
      thumbH = min view (max 8 (view * view / max 1 content))
      thumbY = rectY r + (view - thumbH) * (if maxScroll > 0 then offset / maxScroll else 0)
   in (thumbY, thumbH)

-- | How far one wheel step scrolls.
scrollStep :: Float
scrollStep = lineHeight * 3

-- | An offset moved by this frame's wheel, while the pointer is over the
-- region.
wheelScroll :: Rect -> V2 -> ChibiUI model V2
wheelScroll r offset@(V2 x y) = do
  hov <- hovered r
  V2 dx dy <- scrollDelta
  pure (if hov then V2 (x + dx * scrollStep) (y + dy * scrollStep) else offset)

-- | Keep an offset within what a @content@ extent can scroll through a
-- @view@ extent.
clampScroll :: Float -> Float -> Float -> Float
clampScroll content view = clamp 0 (max 0 (content - view))

-- | A 1px horizontal rule across the line's width.
separator :: ChibiUI model ()
separator = do
  (_, r) <- widgetRect ((\w -> Size w 1) <$> availWidth)
  th <- theme
  fillRectUI r (themeBorder th)

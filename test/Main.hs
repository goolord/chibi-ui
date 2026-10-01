{-# LANGUAGE OverloadedStrings #-}

-- | Headless frame tests: run views against a context with scripted input
-- and check what the widgets did. No window, no GL.
module Main (main) where

import Control.Concurrent (threadDelay)
import Control.Monad (filterM)
import Data.IORef
import Data.List (nub, sortOn)
import Foreign.ForeignPtr (withForeignPtr)
import Foreign.Ptr (Ptr, castPtr)
import Foreign.Storable (peekByteOff)
import qualified Data.Text as T
import System.Info (os)
import ChibiUI
import ChibiUI.Backend

main :: IO ()
main = do
  testCounter
  testModel
  testModelMapping
  testMappedMenu
  testKeyedFields
  testTextInput
  testTextAreaLifecycle
  testTextAreaNavigation
  testTextAreaSelection
  testTextAreaPointer
  testTextAreaViewport
  testIntInput
  testSlider
  testCheckbox
  testRadio
  testCombo
  testTabs
  testLastItem
  testTooltip
  testDisabled
  testLabeled
  testEdit
  testFillWidth
  testPanel
  testTextInputHint
  testDragFloat
  testLabelWrapped
  testDebugOverlay
  testTable
  testRaggedTable
  testPlotLines
  testLayoutCursor
  testScroll
  testNestedScroll
  testDrawData
  testDamage
  testFontScales
  testFieldClipping
  testFieldLifecycle
  testFieldSessions
  testNumberSteps
  testLayoutGroups
  testLayoutTransitions
  testClippedInteraction
  testConstrainedPrimitives
  testSelectionEditing
  testMouseSelection
  testContextMenus
  testKeyboardButtons
  testFocusTransitions
  testTextAlignment
  testDpiGeometry
  testSelectableText
  testTreeInteraction
  testNestedTree
  testKeyedTree
  testClippedTree
  putStrLn "chibi-ui-test: all tests passed"

-- | A context for headless frames.
newTestContext :: IO (Context ())
newTestContext = newContext ()

-- | Run one frame with an input tweak applied to the last frame's input.
frame :: Context model -> (Input -> Input) -> ChibiUI model a -> IO a
frame ctx f view = do
  inp0 <- contextInput ctx
  let inp = f (clearEphemeral inp0)
  (a, _dd) <- runFrame ctx inp view
  pure a

at :: Float -> Float -> Input -> Input
at x y inp = inp {inputMousePos = V2 x y}

pressLeft, releaseLeft :: Input -> Input
pressLeft = applyMouseButton MouseLeft True
releaseLeft = applyMouseButton MouseLeft False

typeText :: Text -> Input -> Input
typeText t inp = inp {inputChars = inputChars inp ++ T.unpack t}

-- | The frame's recorded rects, sorted by x.
readRects :: Context model -> IO [Rect]
readRects ctx = do
  rs <- frameRects ctx
  pure (sortOn rectX (map snd rs))

-- | The tallest recorded rect: in these tests, the interactive widget the
-- test wants to click.
bigRect :: [Rect] -> IO Rect
bigRect rs = case rs of
  [] -> fail "no widget rects recorded"
  _ -> pure (foldr1 (\a b -> if rectH a >= rectH b then a else b) rs)

data Model = Model {modelCount :: Int, modelText :: Text}
  deriving (Eq, Show)

testCounter :: IO ()
testCounter = do
  ctx <- newContext (Model 0 "")
  logRef <- newIORef []
  let view = do
        up <- button "+"
        inp <- ChibiUI.getInput
        liftIO (modifyIORef' logRef ((up, pressedIn MouseLeft inp, releasedIn MouseLeft inp, inputButtonsHeld inp) :))
        when up (modify (\m -> m {modelCount = modelCount m + 1}))
        n <- gets modelCount
        label (T.pack (show n))
  _ <- frame ctx id view
  rs <- readRects ctx
  r <- bigRect rs
  let cx = rectX r + 1
      cy = rectY r + 1
  _ <- frame ctx (at cx cy . pressLeft) view
  _ <- frame ctx (at cx cy . releaseLeft) view
  n <- runChibiUI ctx (gets modelCount)
  unless (n == 1) $ do
    clicks <- readIORef logRef
    fail ("counter: expected 1, got " ++ show n ++ " frames=" ++ show (reverse clicks))

-- Model access is immediate, survives layout scopes and frames, and does
-- not consume widget ids or share state with a different context.
testModel :: IO ()
testModel = do
  ctx <- newContext (Model 3 "seed")
  other <- newContext (Model 9 "other")
  initial <- runChibiUI ctx get
  assert "model: initial value lost" (initial == Model 3 "seed")
  current <- frame ctx id $ do
    column $ withKey "nested" $ modify' (\m -> m {modelCount = modelCount m + 2})
    modify (\m -> m {modelText = modelText m <> "!"})
    get
  assert "model: updates not visible immediately" (current == Model 5 "seed!")
  retained <- frame ctx id get
  isolated <- runChibiUI other get
  assert "model: frame reset or context leaked" (retained == current && isolated == Model 9 "other")
  runChibiUI ctx (put (Model 20 "replaced"))
  replaced <- frame ctx id get
  assert "model: put did not replace the model" (replaced == Model 20 "replaced")
  _ <- frame ctx id (label "same")
  before <- frameRects ctx
  _ <- frame ctx id (get >> put replaced >> label "same")
  after <- frameRects ctx
  assert "model: state access changed widget identity" (before == after)

testModelMapping :: IO ()
testModelMapping = do
  ctx <- newContext (Model 3 "seed")
  let countView = mapModel modelCount (\n m -> m {modelCount = n})
  message <- runChibiUI ctx $ mapMsg show $ countView $ do
    modify (+ 2)
    -- Interleaving a parent update must not be overwritten by a child write.
    liftIO (runChibiUI ctx (modify (\m -> m {modelText = "latest"})))
    put 20
    modify' (+ 1)
    get
  parent <- runChibiUI ctx get
  assert "mapModel: updates were stale, repeated, or not written through"
    (message == "21" && parent == Model 21 "latest")
  runChibiUI ctx (put (Model 42 "replacement"))
  live <- runChibiUI ctx (countView get)
  optional <- runChibiUI ctx (mapMsg (fmap (+ 1)) (pure (Just live)))
  absent <- runChibiUI ctx (mapMsg (fmap (+ 1)) (pure (Nothing :: Maybe Int)))
  assert "mapping: current state or optional messages lost" (optional == Just 43 && absent == Nothing)
  _ <- frame ctx id (label "same")
  before <- frameRects ctx
  _ <- frame ctx id (countView (label "same"))
  after <- frameRects ctx
  assert "mapModel: changed widget identity or layout" (before == after)

-- Nested projections captured by a popup must compose against the current
-- root model, preserving siblings at every level when the action later runs.
testMappedMenu :: IO ()
testMappedMenu = do
  ctx <- newContext (Model 0 "seed", False)
  let view = mapModel fst (\m (_, flag) -> (m, flag)) $
        mapModel modelCount (\n m -> m {modelCount = n}) $ do
          label "target"
          contextMenu [("Increment", modify (+ 1)), ("Replace", put 7)]
          get
      open = do
        _ <- frame ctx (at 12 12 . applyMouseButton MouseRight True) view
        frame ctx (applyMouseButton MouseRight False) view
  _ <- frame ctx id view
  _ <- open
  runChibiUI ctx (put (Model 40 "latest", True))
  result <- frame ctx (at 20 20 . pressLeft) view
  parent <- runChibiUI ctx get
  assert "mapModel: deferred nested update used a snapshot or overwrote siblings"
    (result == 41 && parent == (Model 41 "latest", True))
  _ <- frame ctx releaseLeft view
  _ <- open
  runChibiUI ctx (put (Model 90 "newer", False))
  _ <- frame ctx (keys [KeyEnd]) view
  replaced <- frame ctx (keys [KeyEnter]) view
  final <- runChibiUI ctx get
  assert "mapModel: deferred put replaced the parent or lost nested projection"
    (replaced == 7 && final == (Model 7 "newer", False))

-- Keyed editor drafts and focus still follow their widget across reordering.
testKeyedFields :: IO ()
testKeyedFields = do
  ctx <- newTestContext
  let view = mapM (\key -> withKey key (column (textInput key)))
  _ <- frame ctx (keys [KeyTab]) (view ["a", "b"])
  edited <- frame ctx (typeText "!") (view ["a", "b"])
  reordered <- frame ctx id (view ["b", "a"])
  assert "keys: edit draft or focus lost on reorder" (edited == ["a!", "b"] && reordered == ["b", "a!"])

testTextInput :: IO ()
testTextInput = do
  ctx <- newContext (Model 0 "hi")
  let view = do
        v <- textInput =<< gets modelText
        modify (\m -> m {modelText = v})
  _ <- frame ctx id view
  rs <- readRects ctx
  r <- bigRect rs
  let cx = rectX r + rectW r - 2
      cy = rectY r + 1
  -- Click past the text to focus at its end, then type.
  _ <- frame ctx (at cx cy . pressLeft) view
  _ <- frame ctx (at cx cy . releaseLeft) view
  _ <- frame ctx (typeText "!") view
  v1 <- runChibiUI ctx (gets modelText)
  unless (v1 == "hi!") (fail ("text input: expected \"hi!\", got " ++ show v1))

testTextAreaLifecycle :: IO ()
testTextAreaLifecycle = do
  ctx <- newContext (Model 0 "first\nsecond")
  let view = do
        value <- textArea =<< gets modelText
        modify (\m -> m {modelText = value})
        pure value
      plain = chord noModifiers
  _ <- frame ctx (plain [KeyTab]) view
  newlineValue <- frame ctx (plain [KeyEnter]) view
  typed <- frame ctx (typeText "last" . plain []) view
  assert "textarea: Enter did not insert newline and keep focus"
    (newlineValue == "first\nsecond\n" && typed == "first\nsecond\nlast")
  undone <- frame ctx (chord primaryMods [KeyChar 'z']) view
  undoneNewline <- frame ctx (chord primaryMods [KeyChar 'z']) view
  redone <- frame ctx (chord primaryMods [KeyChar 'y', KeyChar 'y']) view
  assert "textarea: undo/redo lost newline history"
    (undone == newlineValue && undoneNewline == "first\nsecond" && redone == typed)
  escaped <- frame ctx (plain [KeyEscape]) view
  idle <- frame ctx (typeText "ignored" . plain []) view
  assert "textarea: Escape did not restore original and blur" (escaped == "first\nsecond" && idle == escaped)
  _ <- frame ctx (plain [KeyTab]) view
  committed <- frame ctx (typeText "!" . plain []) view
  _ <- frame ctx (at 700 500 . pressLeft) view
  blurred <- frame ctx (typeText "ignored" . releaseLeft) view
  assert "textarea: blur failed to retain live model" (committed == "first\nsecond!" && blurred == committed)
  runChibiUI ctx (modify (\m -> m {modelText = "replacement\r\ntext"}))
  replaced <- frame ctx id view
  assert "textarea: external text or CRLF normalization failed" (replaced == "replacement\ntext")

testTextAreaNavigation :: IO ()
testTextAreaNavigation = do
  let insertAfter mods movements = do
        ctx <- newTestContext
        let view = textArea "abc\nxy\n1234"
        _ <- frame ctx (keys [KeyTab]) view
        _ <- frame ctx (chord mods movements) view
        frame ctx (typeText "!" . chord noModifiers []) view
  up <- insertAfter noModifiers [KeyHome, KeyUp, KeyRight]
  down <- insertAfter noModifiers [KeyHome, KeyUp, KeyDown, KeyEnd]
  start <- insertAfter primaryMods [KeyHome]
  end <- insertAfter primaryMods [KeyHome, KeyEnd]
  assert "textarea: line or document navigation failed"
    (up == "abc\nx!y\n1234" && down == "abc\nxy\n1234!" && start == "!abc\nxy\n1234" && end == down)
  ctx <- newTestContext
  let view = textArea "abc\nxy\n1234"
  _ <- frame ctx (keys [KeyTab]) view
  joined <- frame ctx (keys [KeyHome, KeyBackspace]) view
  assert "textarea: Backspace at line start did not join lines" (joined == "abc\nxy1234")
  _ <- frame ctx (chord primaryMods [KeyChar 'z']) view
  _ <- frame ctx (chord noModifiers [KeyUp, KeyEnd]) view
  deleted <- frame ctx (keys [KeyDelete]) view
  assert "textarea: Delete at line end did not join lines" (deleted == joined)

testTextAreaSelection :: IO ()
testTextAreaSelection = do
  ctx <- newTestContext
  clip <- newIORef Nothing
  withClipboard ctx (readIORef clip) (writeIORef clip . Just)
  let view = textArea "abc\nxy\n1234"
  _ <- frame ctx (keys [KeyTab]) view
  _ <- frame ctx (chord (noModifiers {modShift = True}) [KeyUp]) view
  _ <- frame ctx (chord primaryMods [KeyChar 'c']) view
  copied <- readIORef clip
  cut <- frame ctx (chord primaryMods [KeyChar 'x']) view
  assert "textarea: cross-line copy/cut lost newline" (copied == Just "\n1234" && cut == "abc\nxy")
  _ <- frame ctx (chord primaryMods [KeyChar 'z']) view
  writeIORef clip (Just "A\r\nB\rC\tD")
  pasted <- frame ctx (chord primaryMods [KeyChar 'v']) view
  assert "textarea: multiline paste or replacement failed" (pasted == "abc\nxyA\nB\nC    D")
  -- The shared context menu must use the multiline command path as well.
  _ <- frame ctx (chord primaryMods [KeyChar 'a']) view
  _ <- frame ctx (chord (noModifiers {modShift = True}) [KeyF 10]) view
  _ <- frame ctx (chord noModifiers [KeyEnd]) view -- Select all
  _ <- frame ctx (chord noModifiers [KeyUp]) view -- Paste
  menuPaste <- frame ctx (chord noModifiers [KeyEnter]) view
  assert "textarea: menu Paste lost line breaks" (menuPaste == "A\nB\nC    D")

testTextAreaPointer :: IO ()
testTextAreaPointer = do
  ctx <- newTestContext
  clip <- newIORef Nothing
  withClipboard ctx (readIORef clip) (writeIORef clip . Just)
  let view = textArea "ab\nβγ\nlast"
  a <- runChibiUI ctx (measureText "a")
  beta <- runChibiUI ctx (measureText "β")
  _ <- frame ctx (at (14 + a) 20 . pressLeft) view
  _ <- frame ctx (at (14 + beta) 33) view
  _ <- frame ctx releaseLeft view
  _ <- frame ctx (chord primaryMods [KeyChar 'c']) view
  copied <- readIORef clip
  replaced <- frame ctx (typeText "X" . chord noModifiers []) view
  assert "textarea: drag across lines selected wrong text" (copied == Just "b\nβ" && replaced == "aXγ\nlast")

testTextAreaViewport :: IO ()
testTextAreaViewport = do
  ctx <- newTestContext
  let source = T.intercalate "\n" ["row" <> T.pack (show i) | i <- [0 .. 19 :: Int]]
      view = nextWidth 80 >> nextHeight 36 >> textArea source
      render tweak = do
        input <- contextInput ctx
        (_, dd) <- runFrame ctx (tweak (clearEphemeral input)) view
        points <- verticesFor dd (const True)
        glyphs <- glyphVertices dd
        assert "textarea: painting escaped assigned bounds" (all (inside (Rect 10 10 80 36)) points)
        assert "textarea: glyphs escaped content viewport" (all (inside (Rect 14 14 72 28)) glyphs)
        assert "textarea: visible text disappeared" (not (null glyphs))
  render id
  -- Wheel scrolling works without focus, and hit-testing uses the scrolled line.
  render (\inp -> (at 15 16 inp) {inputScroll = V2 0 1})
  _ <- frame ctx (at 14 16 . pressLeft) view
  _ <- frame ctx releaseLeft view
  edited <- frame ctx (typeText "X") view
  assert "textarea: wheel offset was lost during pointer hit-testing" (T.lines edited !! 3 == "Xrow3")
  render (chord primaryMods [KeyEnd])
  -- End reveals the last line; insertion at the viewport bottom must hit it.
  _ <- frame ctx (at 14 40 . pressLeft . chord noModifiers []) view
  _ <- frame ctx releaseLeft view
  lastLine <- frame ctx (typeText "Y") view
  assert "textarea: caret did not scroll last line into view" (last (T.lines lastLine) == "Yrow19")
  render (chord primaryMods [KeyChar 'a'])
  render (chord primaryMods [KeyHome])
  (_, tiny) <- runFrame ctx emptyInput (nextWidth 2 >> nextHeight 2 >> textArea source)
  tinyGlyphs <- glyphVertices tiny
  assert "textarea: empty viewport emitted text" (null tinyGlyphs)
  hiddenCtx <- newTestContext
  let hidden = nextHeight 10 >> scrollColumn (space 20 >> textArea "hidden\ntext")
  _ <- frame hiddenCtx (at 20 40 . pressLeft) hidden
  untouched <- frame hiddenCtx (typeText "!" . releaseLeft . keys [KeyEnter]) hidden
  assert "textarea: clipped field accepted input" (untouched == "hidden\ntext")

testIntInput :: IO ()
testIntInput = do
  ctx <- newTestContext
  valueRef <- newIORef ([] :: [Int])
  let view = do
        v <- intInput 42
        liftIO (modifyIORef' valueRef (v :))
  _ <- frame ctx id view
  rs <- readRects ctx
  r <- bigRect rs
  let cx = rectX r + rectW r - 2
      cy = rectY r + 1
  _ <- frame ctx (at cx cy . pressLeft) view
  _ <- frame ctx (at cx cy . releaseLeft) view
  -- Click past "42", backspace -> "4", type 3 -> "43".
  _ <- frame ctx (\i -> i {inputKeys = inputKeys i ++ [KeyBackspace]}) view
  _ <- frame ctx (typeText "3") view
  vs <- readIORef valueRef
  unless (reverse vs == [42, 42, 42, 4, 43]) (fail ("int input: expected [42,42,42,4,43], got " ++ show (reverse vs)))

testTable :: IO ()
testTable = do
  ctx <- newTestContext
  selRef <- newIORef (Nothing :: Maybe Int)
  let headers = ["a", "b"]
      rows = [["1", "2"], ["3", "4"], ["5", "6"]]
      view = do
        s <- table headers rows
        liftIO (writeIORef selRef s)
  _ <- frame ctx id view
  rs <- readRects ctx
  r <- bigRect rs
  -- Click the second data row: header (16 + 2*3) plus two rows down.
  let rowH = 16 + 2 * 3
      cx = rectX r + 1
      cy = rectY r + rowH * 2 + 1
  _ <- frame ctx (at cx cy . pressLeft) view
  _ <- frame ctx (at cx cy . releaseLeft) view
  s <- readIORef selRef
  unless (s == Just 1) (fail ("table: expected row 1 selected, got " ++ show s))

-- Missing cells render as blanks; extra cells do not create columns.
testRaggedTable :: IO ()
testRaggedTable = do
  ctx <- newTestContext
  let draw rows = do
        (_, dd) <- runFrame ctx emptyInput (table ["a", "b"] rows)
        rs <- readRects ctx
        points <- verticesFor dd (const True)
        pure (rs, points, drawCommands dd)
  ragged <- draw [["one"], [], ["two", "three", "ignored"]]
  padded <- draw [["one", ""], ["", ""], ["two", "three"]]
  assert "table: ragged rows differ from padded rows" (ragged == padded)
  empty <- frame ctx id (table [] [])
  assert "table: empty table has a selection" (empty == Nothing)

-- Dragging, clicking, clamping and keyboard steps all move the value;
-- release keeps it, and only the focused slider takes arrow keys.
testSlider :: IO ()
testSlider = do
  ctx <- newTestContext
  ref <- newIORef (30 :: Float)
  let view = do
        v0 <- liftIO (readIORef ref)
        v <- slider v0 0 100
        liftIO (writeIORef ref v)
        pure v
  _ <- frame ctx id view
  rs <- readRects ctx
  r <- bigRect rs
  -- The track ends where the value's room, sized for the wider end, begins.
  valueW <- runChibiUI ctx (max <$> measureText "0.00" <*> measureText "100.00")
  let cy = rectY r + rectH r / 2
      trackW = rectW r - valueW - 6
      atFrac f = at (rectX r + trackW * f) cy
  clicked <- frame ctx (atFrac 0.75 . pressLeft) view
  released <- frame ctx (atFrac 0.75 . releaseLeft) view
  assert "slider: click did not position the thumb" (abs (clicked - 75) <= 1 && released == clicked)
  _ <- frame ctx (atFrac 0.5 . pressLeft) view
  dragged <- frame ctx (atFrac 0.1) view
  clamped <- frame ctx (at (-40) cy) view
  settled <- frame ctx (at (-40) cy . releaseLeft) view
  assert "slider: drag or clamping failed"
    (abs (dragged - 10) <= 1 && clamped == 0 && settled == 0)
  _ <- frame ctx (atFrac 0.5 . pressLeft) view
  _ <- frame ctx (atFrac 0.5 . releaseLeft) view
  left <- frame ctx (keys [KeyLeft]) view
  right <- frame ctx (keys [KeyRight]) view
  idle <- frame ctx id view
  assert "slider: keyboard steps failed"
    (abs (left - 40) <= 1 && abs (right - 50) <= 1 && idle == right)
  -- Unfocused arrows leave the value alone.
  blurred <- frame ctx (at 500 400 . pressLeft) view
  outside <- frame ctx (keys [KeyRight] . releaseLeft) view
  assert "slider: arrows reached an unfocused slider" (blurred == right && outside == right)

-- A click flips a checkbox, and so does Space while it is focused.
testCheckbox :: IO ()
testCheckbox = do
  ctx <- newTestContext
  ref <- newIORef False
  let view = do
        v <- checkbox "on" =<< liftIO (readIORef ref)
        liftIO (writeIORef ref v)
        pure v
  _ <- frame ctx id view
  r <- bigRect =<< readRects ctx
  pressed <- frame ctx (at (rectX r + 2) (rectY r + 2) . pressLeft) view
  clicked <- frame ctx (at (rectX r + 2) (rectY r + 2) . releaseLeft) view
  spaced <- frame ctx (keys [KeySpace]) view
  idle <- frame ctx id view
  assert "checkbox: click or Space did not flip it" (not pressed && clicked && not spaced && not idle)

-- Clicking an option chooses it; the options lie left to right.
testRadio :: IO ()
testRadio = do
  ctx <- newTestContext
  ref <- newIORef 'a'
  let view = do
        v <- radio [("a", 'a'), ("b", 'b'), ("c", 'c')] =<< liftIO (readIORef ref)
        liftIO (writeIORef ref v)
        pure v
  _ <- frame ctx id view
  rs <- readRects ctx
  assert ("radio: expected three options in a row: " ++ show rs)
    (length rs == 3 && length (nub (map rectY rs)) == 1)
  chosen <- clickIn ctx (last rs) view
  kept <- frame ctx id view
  assert "radio: click did not choose the option" (chosen == 'c' && kept == 'c')

-- A click opens the combo's menu, and a press on an option chooses it.
testCombo :: IO ()
testCombo = do
  ctx <- newTestContext
  ref <- newIORef 'a'
  let view = do
        v <- combo [("a", 'a'), ("b", 'b'), ("c", 'c')] =<< liftIO (readIORef ref)
        liftIO (writeIORef ref v)
        pure v
  _ <- frame ctx id view
  r <- bigRect =<< readRects ctx
  opened <- clickIn ctx r view
  -- The menu opens under the combo, its rows 26 tall below a 1px border.
  let second = at (rectX r + 10) (rectY r + rectH r + 1 + 26 * 1.5)
  picked <- frame ctx (second . pressLeft) view
  _ <- frame ctx (second . releaseLeft) view
  kept <- frame ctx id view
  assert ("combo: menu did not choose: " ++ show (opened, picked, kept))
    (opened == 'a' && picked == 'b' && kept == 'b')

-- Clicking a tab header selects it.
testTabs :: IO ()
testTabs = do
  ctx <- newTestContext
  ref <- newIORef 0
  let view = do
        v <- tabs ["one", "two", "three"] =<< liftIO (readIORef ref)
        liftIO (writeIORef ref v)
        pure v
  _ <- frame ctx id view
  rs <- readRects ctx
  selected <- clickIn ctx (last rs) view
  kept <- frame ctx id view
  assert "tabs: click did not select the tab" (length rs == 3 && selected == 2 && kept == 2)

-- Last-item queries answer for the widget or group placed just before.
testLastItem :: IO ()
testLastItem = do
  ctx <- newTestContext
  let view = do
        _ <- button "first"
        r <- itemRect
        hov <- itemHovered
        _ <- row (button "a" >> button "b")
        rowFocused <- itemFocused
        rowActive <- itemActive
        pure (r, hov, rowFocused, rowActive)
  (r, idle, _, _) <- frame ctx id view
  (_, over, _, _) <- frame ctx (at (rectX r + 2) (rectY r + 2)) view
  assert "last item: hover wrong for the first button" (not idle && over)
  rs <- readRects ctx
  let b = last rs
      onB = at (rectX b + 2) (rectY b + 2)
  (_, _, focusedPress, activePress) <- frame ctx (onB . pressLeft) view
  _ <- frame ctx (onB . releaseLeft) view
  (_, _, focusedAfter, activeAfter) <- frame ctx id view
  assert "last item: group did not report its pressed, focused child"
    (activePress && focusedPress && focusedAfter && not activeAfter)

-- A tooltip shows once the pointer has rested on its item, and goes when
-- the pointer leaves.
testTooltip :: IO ()
testTooltip = do
  ctx <- newTestContext
  let view = button "hover me" >> tooltip "tip"
      quads f = do
        inp0 <- contextInput ctx
        (_, dd) <- runFrame ctx (f (clearEphemeral inp0)) view
        pure (sum (map cmdQuadCount (drawCommands dd)))
  plain <- quads id
  r <- bigRect =<< readRects ctx
  let over = at (rectX r + 2) (rectY r + 2)
  early <- quads over
  threadDelay 600000
  shown <- quads over
  gone <- quads (at 500 400)
  assert ("tooltip: wrong quads: " ++ show (plain, early, shown, gone))
    (early == plain && shown > plain && gone == plain)

-- A disabled button neither clicks nor takes focus, by pointer or Tab;
-- enabled again, it does both.
testDisabled :: IO ()
testDisabled = do
  ctx <- newTestContext
  off <- newIORef True
  let view = do
        d <- liftIO (readIORef off)
        clicked <- disabled d (button "go")
        focused <- itemFocused
        pure (clicked, focused)
  _ <- frame ctx id view
  r <- bigRect =<< readRects ctx
  (offClick, _) <- clickIn ctx r view
  (_, offTab) <- frame ctx (keys [KeyTab]) view >> frame ctx id view
  writeIORef off False
  (onClick, _) <- clickIn ctx r view
  (_, onFocus) <- frame ctx id view
  assert "disabled: button reacted while disabled, or not once enabled"
    (not offClick && not offTab && onClick && onFocus)

-- A labeled widget sits right of its caption, on one line as tall as a
-- field.
testLabeled :: IO ()
testLabeled = do
  ctx <- newTestContext
  _ <- frame ctx id (labeled "caption" (textInput "value"))
  rs <- readRects ctx
  case rs of
    [caption, field] -> assert ("labeled: caption and field misplaced: " ++ show rs)
      (rectX caption < rectX field && rectY caption == rectY field && rectH caption == rectH field)
    _ -> fail ("labeled: expected two rects: " ++ show rs)

-- 'edit' feeds a widget part of the model and keeps what it returns.
testEdit :: IO ()
testEdit = do
  ctx <- newContext (Model 1 "keep")
  let view = edit modelCount (\v m -> m {modelCount = v}) (\n -> pure (n * 10))
  shown <- frame ctx id view
  after <- runChibiUI ctx get
  assert "edit: value not shown or kept" (shown == 10 && after == Model 10 "keep")

-- 'fillWidth' stretches the next widget to the right edge of its scope,
-- here the window less its padding.
testFillWidth :: IO ()
testFillWidth = do
  ctx <- newTestContext
  _ <- frame ctx id (labeled "caption" (fillWidth >> textInput "value"))
  rs <- readRects ctx
  pad <- themeWindowPad <$> runChibiUI ctx theme
  let field = last rs
  assert ("fillWidth: field does not reach the edge: " ++ show rs)
    (rectX field + rectW field == 800 - pad)

-- A panel spans the line and keeps its title and body a gap inside its
-- border on every side.
testPanel :: IO ()
testPanel = do
  ctx <- newTestContext
  outer <- frame ctx id (panel "box" (button "x") >> itemRect)
  rs <- readRects ctx
  th <- runChibiUI ctx theme
  let gap = themeGap th
      left = minimum (map rectX rs)
      top = minimum (map rectY rs)
      bottom = maximum (map (\r -> rectY r + rectH r) rs)
  assert ("panel: padding or width wrong: " ++ show (outer, rs))
    (rectW outer == 800 - themeWindowPad th * 2
      && left - rectX outer == gap && top - rectY outer == gap
      && rectY outer + rectH outer - bottom == gap)

-- An empty, unfocused field draws its hint; typing hides it.
testTextInputHint :: IO ()
testTextInputHint = do
  ctx <- newTestContext
  ref <- newIORef ""
  let view = do
        v <- textInputHint "hint" =<< liftIO (readIORef ref)
        liftIO (writeIORef ref v)
      quads f = do
        inp0 <- contextInput ctx
        (_, dd) <- runFrame ctx (f (clearEphemeral inp0)) view
        pure (sum (map cmdQuadCount (drawCommands dd)))
  withHint <- quads id
  (_, plain) <- runFrame ctx emptyInput (withKey "plain" (textInput ""))
  let plainQuads = sum (map cmdQuadCount (drawCommands plain))
  _ <- quads (keys [KeyTab])
  _ <- quads (typeText "x")
  typed <- readIORef ref
  focusedEmpty <- quads (keys [KeyBackspace])
  assert ("hint: drawn wrong: " ++ show (withHint, plainQuads, typed, focusedEmpty))
    (withHint > plainQuads && typed == "x" && focusedEmpty < withHint)

-- Dragging a number changes it by its speed per pixel moved since the
-- press, even past its box; arrows step it while focused.
testDragFloat :: IO ()
testDragFloat = do
  ctx <- newTestContext
  ref <- newIORef (1 :: Float)
  let view = do
        v <- (`dragFloat` 0.5) =<< liftIO (readIORef ref)
        liftIO (writeIORef ref v)
        pure v
  _ <- frame ctx id view
  r <- bigRect =<< readRects ctx
  let y = rectY r + 2
      x0 = rectX r + 4
  pressed <- frame ctx (at x0 y . pressLeft) view
  dragged <- frame ctx (at (x0 + 10) y) view
  far <- frame ctx (at (x0 + 400) y) view
  released <- frame ctx (at (x0 + 400) y . releaseLeft) view
  stepped <- frame ctx (keys [KeyLeft]) view
  assert ("dragFloat: wrong values: " ++ show (pressed, dragged, far, released, stepped))
    (pressed == 1 && dragged == 6 && far == 201 && released == 201 && stepped == 200.5)

-- Wrapped text takes a line per wrapped line, each within the width, and
-- a newline always breaks.
testLabelWrapped :: IO ()
testLabelWrapped = do
  ctx <- newTestContext
  let text = "one two three four five six seven"
  widest <- runChibiUI ctx (measureText "three")
  _ <- frame ctx id (nextWidth (widest * 2) >> labelWrapped text)
  wrapped <- bigRect =<< readRects ctx
  _ <- frame ctx id (labelWrapped "short\nlines")
  broken <- bigRect =<< readRects ctx
  assert ("labelWrapped: wrong line counts: " ++ show (wrapped, broken))
    (rectW wrapped == widest * 2 && rectH wrapped >= 16 * 3 && rectH broken == 16 * 2)

-- The debug overlay draws outlines, the hovered widget's label and the
-- counts on top of the view, and places no widget of its own.
testDebugOverlay :: IO ()
testDebugOverlay = do
  ctx <- newTestContext
  let quads view = do
        (_, dd) <- runFrame ctx (at 12 12 emptyInput) view
        n <- length <$> readRects ctx
        pure (sum (map cmdQuadCount (drawCommands dd)), n)
  (plain, plainRects) <- quads (button "a" >> button "b" >> pure ())
  (debug, debugRects) <- quads (button "a" >> button "b" >> debugOverlay)
  assert ("debugOverlay: drew nothing or placed widgets: " ++ show (plain, debug))
    (debug > plain + 8 && debugRects == plainRects)

-- | Press and release the left button just inside a rect: the release
-- frame's result.
clickIn :: Context model -> Rect -> ChibiUI model a -> IO a
clickIn ctx r view = do
  let here = at (rectX r + 2) (rectY r + 2)
  _ <- frame ctx (here . pressLeft) view
  frame ctx (here . releaseLeft) view

-- A wave draws more than a flat line, which draws more than nothing; every
-- series stays inside the plot's rect.
testPlotLines :: IO ()
testPlotLines = do
  ctx <- newTestContext
  let draw values = do
        (_, dd) <- runFrame ctx emptyInput (nextWidth 100 >> nextHeight 50 >> plotLines values)
        points <- verticesFor dd (const True)
        r <- bigRect =<< readRects ctx
        pure (length points, all (inside r) points)
      wave = [sin (fromIntegral i * 0.5) | i <- [0 .. 39 :: Int]]
  (waveCount, waveInside) <- draw wave
  (flatCount, _) <- draw [5, 5, 5]
  (emptyCount, _) <- draw []
  (singleCount, _) <- draw [3]
  assert "plot: wave, flat or empty series drew wrong amounts"
    (waveCount >= flatCount && flatCount > emptyCount && singleCount > emptyCount)
  -- However steep, a line costs at most a quad per pixel column.
  assert "plot: more than one quad per pixel column"
    ((waveCount - emptyCount) `div` 4 <= 100)
  assert "plot: geometry escaped its rect" waveInside

testLayoutCursor :: IO ()
testLayoutCursor = do
  ctx <- newTestContext
  let view = do
        label "a"
        sameLine
        label "bb"
        newline
        label "ccc"
  _ <- frame ctx id view
  rs <- readRects ctx
  let pad = 10 -- defaultTheme's windowPad
      line1 = [r | r <- rs, rectY r == pad]
      below = [r | r <- rs, rectY r > pad]
  unless (length rs == 3) (fail ("layout: expected three rects, got " ++ show (length rs)))
  unless (length line1 == 2) (fail "layout: expected two labels on the first line")
  case line1 of
    [a, b] -> do
      unless (rectX a < rectX b) (fail "layout: sameLine did not advance rightward")
      unless (rectX b >= rectX a + rectW a) (fail "layout: sameLine overlapped the first label")
    _ -> fail "layout: bad first line"
  case below of
    [c] -> unless (rectY c >= pad + 16) (fail "layout: newline did not step down")
    _ -> fail "layout: expected one label on the second line"

testScroll :: IO ()
testScroll = do
  ctx <- newTestContext
  let view = scrollColumn (mapM_ (label . ("row " <>) . T.pack . show) [1 .. 40 :: Int])
      childTops = do
        rs <- readRects ctx
        -- The region is the tallest rect; the labels are the rest.
        let r = foldr1 (\a b -> if rectH a >= rectH b then a else b) rs
        pure (minimum (map rectY [r2 | r2 <- rs, r2 /= r]))
  _ <- frame ctx id view
  top0 <- childTops
  -- Wheel down over the region: the body moves in the input frame.
  rs <- readRects ctx
  let r = foldr1 (\a b -> if rectH a >= rectH b then a else b) rs
      cx = rectX r + rectW r / 2
      cy = rectY r + 10
      step = 3 * 16 -- three line heights
  _ <- frame ctx (\i -> (at cx cy i) {inputScroll = V2 0 1}) view
  top1 <- childTops
  -- A full-window region reaches the window's edges and keeps the padding
  -- inside, above its body.
  pad <- themeWindowPad <$> runChibiUI ctx theme
  let origin = rectY r + pad
  unless (rectY r == 0 && rectX r == 0) (fail ("scroll: top-level region kept the window padding: " ++ show r))
  unless (top0 == origin) (fail ("scroll: first child not at the region's padded top: " ++ show top0))
  unless (top1 == origin - step) (fail ("scroll: expected the body to shift up " ++ show step ++ ", moved to " ++ show top1))
  -- Wheeling back up clamps at zero.
  _ <- frame ctx (\i -> (at cx cy i) {inputScroll = V2 0 (-5)}) view
  _ <- frame ctx id view
  top2 <- childTops
  unless (top2 == origin) (fail ("scroll: did not clamp back to the top: " ++ show top2))
  -- Pressing the track's bottom brings the thumb there: fully scrolled,
  -- so the wheel moves the body no further.
  let barX = rectX r + rectW r - 2
      bottom = rectY r + rectH r - 1
  _ <- frame ctx (at barX bottom . pressLeft) view
  top3 <- childTops
  unless (top3 < origin) (fail ("scroll: track press did not scroll: " ++ show top3))
  _ <- frame ctx (\i -> (at barX bottom i) {inputScroll = V2 0 1}) view
  top4 <- childTops
  unless (top4 == top3) (fail ("scroll: track press did not reach the end: " ++ show (top3, top4)))
  -- Dragging holds the thumb even with the pointer off the bar, and
  -- dragging above the region clamps at the top.
  _ <- frame ctx (at cx (rectY r + rectH r / 2)) view
  top5 <- childTops
  unless (top3 < top5 && top5 < origin) (fail ("scroll: drag to the middle landed at " ++ show top5))
  _ <- frame ctx (at cx (rectY r - 100)) view
  _ <- frame ctx (at cx (rectY r - 100) . releaseLeft) view
  top6 <- childTops
  unless (top6 == origin) (fail ("scroll: drag to the top landed at " ++ show top6))
  -- Released, the pointer moving over the bar no longer scrolls.
  _ <- frame ctx (at barX bottom) view
  top7 <- childTops
  unless (top7 == origin) (fail ("scroll: released drag still scrolled: " ++ show top7))

-- | The wheel scrolls the innermost region under the pointer, and the one
-- around it once the inner region reaches its end.
testNestedScroll :: IO ()
testNestedScroll = do
  ctx <- newTestContext
  let rows prefix n = mapM_ (label . (prefix <>) . T.pack . show) [1 .. n :: Int]
      view = scrollColumn $ do
        rows "above " 5
        nextHeight 60
        scrollColumn (rows "inner " 20)
        rows "below " 40
      innerRegion = do
        rs <- readRects ctx
        case [r | r <- rs, rectH r == 60] of
          [r] -> pure r
          _ -> fail "nested scroll: inner region not found"
      wheel n r = frame ctx (\i -> (at (rectX r + 20) (rectY r + 20) i) {inputScroll = V2 0 n}) view
      step = 3 * 16 -- three line heights
  _ <- frame ctx id view
  inner0 <- innerRegion
  before <- frameRects ctx
  _ <- wheel 1 inner0
  after <- frameRects ctx
  let moved = [(r0, r1) | (k, r0) <- before, Just r1 <- [lookup k after], r0 /= r1]
  assert ("nested scroll: the wheel moved more than the inner body: " ++ show (length moved))
    (length moved == 20)
  assert "nested scroll: the inner body did not move by one step"
    (all (\(r0, r1) -> rectY r1 == rectY r0 - step) moved)
  -- At its end, the inner region hands the wheel to the outer one.
  _ <- wheel 100 inner0
  inner1 <- innerRegion
  assert "nested scroll: the outer region moved with the inner one" (inner1 == inner0)
  _ <- wheel 1 inner1
  inner2 <- innerRegion
  assert ("nested scroll: the outer region did not take the wheel: " ++ show (inner1, inner2))
    (rectY inner2 == rectY inner1 - step)

-- | Fonts survive the session's startup sequence (a second font replacing
-- the context's) and scale changes rebuild cleanly. A regression here once
-- left a dangling font handle that crashed on the first scale change.
testFontScales :: IO ()
testFontScales = do
  ctx <- newTestContext
  -- The session creates a font of its own and installs it over the
  -- context's; both records must stay usable memory.
  font2 <- newFont embeddedFont
  setFont ctx font2
  let w = label "hello scaled world"
  renders <-
    mapM
      ( \s -> do
          setScale ctx s
          (_, dd) <- runFrame ctx (emptyInput {inputWindowSize = Size 400 400}) w
          glyphs <- glyphVertices dd
          pure (drawVertexCount dd > 0 && not (null glyphs))
      )
      [1.0, 2.0, 1.5, 1.0]
  unless (and renders) (fail "font scales: a frame at some scale drew no text")
  -- The first font, replaced but never freed, still measures.
  font3 <- newFont embeddedFont
  setFont ctx font3
  (_, dd3) <- runFrame ctx (emptyInput {inputWindowSize = Size 400 400}) (label "again")
  unless (drawVertexCount dd3 > 0) (fail "font scales: replacement font drew nothing")

testDrawData :: IO ()
testDrawData = do
  ctx <- newTestContext
  (_, dd) <- runFrame ctx emptyInput (label "hello")
  unless (drawVertexCount dd >= 4 && drawVertexCount dd `mod` 4 == 0) $
    fail "draw data: no whole quads for a label"
  when (null (drawCommands dd)) (fail "draw data: no commands")
  -- Glyph commands sample the atlas texture.
  unless (any ((== texAtlas) . cmdTextureId) (drawCommands dd)) $
    fail "draw data: no atlas command"
  -- The commands tile the quads in order, from the first: the renderer
  -- draws each through its fixed index buffer, so a gap or overlap would
  -- skip or double-draw quads with every count still looking right.
  let tiles = [(fromIntegral (cmdFirstQuad c), fromIntegral (cmdQuadCount c)) | c <- drawCommands dd] :: [(Int, Int)]
  unless (map fst tiles == scanl (+) 0 (map snd (init tiles)) && sum (map snd tiles) == drawVertexCount dd `div` 4) $
    fail ("draw data: commands do not tile the quads " ++ show tiles)
  -- The first glyph's quad starts at the label's origin, within a glyph's
  -- bearings: the pen is at the content origin and the raster is the font's.
  (x, y) <- withForeignPtr (drawVertices dd) $ \p -> do
    vx <- peekByteOff (castPtr p :: Ptr Float) 0 :: IO Float
    vy <- peekByteOff (castPtr p :: Ptr Float) 4 :: IO Float
    pure (vx, vy)
  unless (x >= 10 && x < 12 && y >= 9 && y < 14) $
    fail ("draw data: first glyph at " ++ show (x, y) ++ ", expected near (10,10)")
  -- Every atlas quad is flat (all UVs -1) or a glyph whose UVs lie inside
  -- the atlas; a misread struct once produced v ~ 5e7, clamping every
  -- sample to an empty atlas row.
  let atlasVertices =
        [ v
        | c <- drawCommands dd
        , cmdTextureId c == texAtlas
        , v <- [fromIntegral (cmdFirstQuad c) * 4 .. fromIntegral (cmdFirstQuad c + cmdQuadCount c) * 4 - 1]
        ] :: [Int]
  badUv <-
    withForeignPtr (drawVertices dd) $ \p ->
      filterM
        ( \v -> do
            u <- peekByteOff (castPtr p :: Ptr Float) (v * 32 + 24) :: IO Float
            v' <- peekByteOff (castPtr p :: Ptr Float) (v * 32 + 28) :: IO Float
            pure (not ((u == -1 && v' == -1) || (u >= 0 && u <= 1 && v' >= 0 && v' <= 1)))
        )
        atlasVertices
  unless (null badUv) (fail ("draw data: atlas UVs neither flat nor in the atlas at vertices " ++ show (take 3 badUv)))

testDamage :: IO ()
testDamage = do
  ctx <- newTestContext
  snapRef <- newIORef Nothing
  let view = do
        label "hello"
        _ <- button "ok"
        pure ()
      step tweak = do
        inp0 <- contextInput ctx
        let inp = tweak (clearEphemeral inp0)
        (_, dd) <- runFrame ctx inp view
        trackFrame snapRef (inputWindowSize inp) dd
      -- Whether small lies within big, with a pixel of slack.
      within pad big small =
        rectX small >= rectX big - pad
          && rectY small >= rectY big - pad
          && rectX small + rectW small <= rectX big + rectW big + pad
          && rectY small + rectH small <= rectY big + rectH big + pad
  -- The first frame owes everything.
  d0 <- step id
  assert "damage: the first frame is a full frame" (d0 == DamageFull)
  -- An unchanged frame owes nothing: the host can idle.
  d1 <- step id
  assert "damage: an unchanged frame is damage-free" (d1 == DamageNone)
  -- Hovering the button repaints only around the button.
  rs <- readRects ctx
  r <- bigRect rs
  d2 <- step (at (rectX r + rectW r / 2) (rectY r + rectH r / 2))
  ok2 <- case d2 of
    DamageRects ds -> pure (not (null ds) && all (within 6 r) ds)
    _ -> pure False
  assert "damage: hover damage stays at the button" ok2
  -- The hover holds: the next identical frame is damage-free again.
  d3 <- step id
  assert "damage: a held hover settles" (d3 == DamageNone)
  -- A resize that moves nothing leaves the draw list as it was; the
  -- renderer repaints in full on a framebuffer size change by itself.
  d4 <- step (\i -> i {inputWindowSize = Size 1024 768})
  assert "damage: a resize that moves nothing is damage-free" (d4 == DamageNone)

assert :: String -> Bool -> IO ()
assert message ok = unless ok (fail message)

keys :: [Key] -> Input -> Input
keys ks inp = inp {inputKeys = ks}

-- Test actual clipped geometry, not just the presence of a scissor command.
verticesFor :: DrawData -> (DrawCmd -> Bool) -> IO [(Float, Float)]
verticesFor dd select = concatMap snd <$> quadsFor dd select

-- The glyph quads' corners: atlas quads whose UVs are not the flat marker.
glyphVertices :: DrawData -> IO [(Float, Float)]
glyphVertices dd = do
  quads <- quadsFor dd ((== texAtlas) . cmdTextureId)
  pure (concat [corners | (u, corners) <- quads, u >= 0])

-- Each quad of the selected commands: its first vertex's U, and its four
-- corners.
quadsFor :: DrawData -> (DrawCmd -> Bool) -> IO [(Float, [(Float, Float)])]
quadsFor dd select = withForeignPtr (drawVertices dd) $ \vertices -> do
  let quads = concat
        [ [fromIntegral (cmdFirstQuad c) .. fromIntegral (cmdFirstQuad c + cmdQuadCount c) - 1]
        | c <- drawCommands dd, select c
        ] :: [Int]
      corner v = (,) <$> peekByteOff vertices (v * 32) <*> peekByteOff vertices (v * 32 + 4)
  mapM (\q -> (,) <$> peekByteOff vertices (q * 128 + 24) <*> mapM corner [q * 4 .. q * 4 + 3]) quads

inside :: Rect -> (Float, Float) -> Bool
inside r (x, y) = x >= rectX r && y >= rectY r
  && x <= rectX r + rectW r && y <= rectY r + rectH r

testFieldClipping :: IO ()
testFieldClipping = do
  ctx <- newTestContext
  let text = T.replicate 20 "wide text "
      view = nextWidth 60 >> textInput text
  (_, idle) <- runFrame ctx emptyInput view
  rs <- readRects ctx
  r <- bigRect rs
  assert "field: nextWidth was overwritten" (rectW r == 60)
  let inner = Rect (rectX r + 4) (rectY r + 4) (rectW r - 8) (rectH r - 8)
      check dd = do
        points <- glyphVertices dd
        allPoints <- verticesFor dd (const True)
        assert "field: missing visible text" (not (null points))
        assert "field: text escaped fixed content clip" (all (inside inner) points)
        assert "field: caret escaped field bounds" (all (inside r) allPoints)
  check idle
  (_, focused) <- runFrame ctx (at 12 12 (pressLeft emptyInput)) view
  check focused
  (_, home) <- runFrame ctx (keys [KeyHome] emptyInput) view
  check home
  (_, tiny) <- runFrame ctx emptyInput (nextWidth 2 >> nextHeight 2 >> textInput text)
  points <- glyphVertices tiny
  assert "field: empty content clip emitted glyphs" (null points)

testFieldLifecycle :: IO ()
testFieldLifecycle = do
  ctx <- newTestContext
  ref <- newIORef "seed"
  let view = do
        value <- liftIO (readIORef ref)
        result <- textInput value
        liftIO (writeIORef ref result)
        pure result
  _ <- frame ctx id view
  -- Tab must enter the focus order even when no widget is focused.
  _ <- frame ctx (keys [KeyTab]) view
  changed <- frame ctx (typeText "!") view
  assert "focus: Tab did not enter first field" (changed == "seed!")
  (escaped, dd) <- runFrame ctx (keys [KeyEscape] emptyInput) view
  assert "field: Escape did not restore focus-time value" (escaped == "seed")
  assert "field: Escape frame disappeared" (drawVertexCount dd > 0)
  _ <- frame ctx (at 170 12 . pressLeft) view
  _ <- frame ctx releaseLeft view
  _ <- frame ctx (typeText "?") view
  committed <- frame ctx (keys [KeyEnter]) view
  after <- frame ctx (typeText "ignored") view
  assert "field: Enter failed to commit and blur" (committed == "seed?" && after == committed)
  _ <- frame ctx (at 12 12 . pressLeft) view
  _ <- frame ctx releaseLeft view
  _ <- frame ctx (at 500 400 . pressLeft) view
  (_, blurred) <- runFrame ctx emptyInput view
  assert "field: blur frame disappeared" (drawVertexCount blurred > 0)
  ctx2 <- newTestContext
  let two = (,) <$> textInput "first" <*> textInput "second"
  _ <- frame ctx2 id two
  _ <- frame ctx2 (\i -> (keys [KeyTab] i) {inputModifiers = (inputModifiers i) {modShift = True}}) two
  pair <- frame ctx2 (\i -> (typeText "!" i) {inputModifiers = inputModifiers emptyInput}) two
  assert "focus: Shift-Tab did not enter last field" (pair == ("first", "second!"))

-- A focus session owns its original value independently of caller updates;
-- only reentry reconciles the supplied value with retained editor history.
testFieldSessions :: IO ()
testFieldSessions = do
  ctx <- newTestContext
  let plain = chord noModifiers
      view = textInput
  _ <- frame ctx (plain [KeyTab]) (view "seed")
  _ <- frame ctx (typeText "!") (view "seed")
  draft <- frame ctx (typeText "?") (view "external")
  cancelled <- frame ctx (plain [KeyEscape]) (view "external")
  assert "field session: caller update replaced draft or cancellation target"
    (draft == "seed!?" && cancelled == "seed")
  _ <- frame ctx (plain [KeyTab]) (view "external")
  fresh <- frame ctx (chord primaryMods [KeyChar 'z']) (view "external")
  assert "field session: new supplied value inherited stale undo history" (fresh == "external")
  _ <- frame ctx (typeText "#" . plain []) (view "external")
  committed <- frame ctx (plain [KeyEnter]) (view "external")
  _ <- frame ctx (plain [KeyTab]) (view committed)
  undone <- frame ctx (chord primaryMods [KeyChar 'z']) (view committed)
  restored <- frame ctx (plain [KeyEscape]) (view committed)
  assert "field session: reentry lost history or kept previous cancellation target"
    (committed == "external#" && undone == "external" && restored == committed)

testNumberSteps :: IO ()
testNumberSteps = do
  ctx <- newTestContext
  let view = intInput 42
  _ <- frame ctx (at 12 12 . pressLeft) view
  up <- frame ctx (keys [KeyUp] . releaseLeft) view
  settled <- frame ctx id view
  down <- frame ctx (keys [KeyDown]) view
  edited <- frame ctx (keys [KeyBackspace]) view
  assert "integer: step lost draft or used stale caller value"
    (up == 43 && settled == 43 && down == 42 && edited == 4)
  ctx2 <- newTestContext
  let floatView = floatInput 20.5
  _ <- frame ctx2 (at 12 12 . pressLeft) floatView
  f <- frame ctx2 (keys [KeyUp] . releaseLeft) floatView
  f' <- frame ctx2 id floatView
  shifted <- frame ctx2 (\i -> (keys [KeyDown] i) {inputModifiers = (inputModifiers i) {modShift = True}}) floatView
  assert "float: stepping or draft persistence failed" (f == 21.5 && f' == f && shifted == 11.5)

testLayoutGroups :: IO ()
testLayoutGroups = do
  ctx <- newTestContext
  let box w h = image 0 w h
      view = do
        row $ do
          column $ box 30 10 >> box 50 20
          column $ box 40 40 >> box 20 5
          box 10 10
        box 5 5
  _ <- frame ctx id view
  rs <- readRects ctx
  let gap = themeGap defaultTheme
      expected = [Rect 10 10 30 10, Rect 10 (20 + gap) 50 20,
                  Rect (60 + gap) 10 40 40, Rect (60 + gap) (50 + gap) 20 5,
                  Rect (100 + 2 * gap) 10 10 10, Rect 10 (55 + 2 * gap) 5 5]
  assert ("layout: nested groups overlap: " ++ show rs) (all (`elem` rs) expected)
  _ <- frame ctx id $ do
    box 20 40
    sameLine
    box 10 10
    box 5 5
  rs2 <- readRects ctx
  assert "layout: sameLine leaked or lost tallest sibling" (Rect 10 (50 + gap) 5 5 `elem` rs2)
  (available, restored) <- frame ctx id $ do
    a <- indent 20 $ do
      box 15 10
      newline
      availWidth
    box 5 5
    b <- availWidth
    pure (a, b)
  rs3 <- readRects ctx
  assert "layout: indent did not move first child" (Rect 30 10 15 10 `elem` rs3)
  assert "layout: indent did not restore width" (restored - available == 20)
  (remaining, full) <- frame ctx id $ do
    a <- row $ box 50 10 >> availWidth
    b <- availWidth
    pure (a, b)
  assert "layout: available width ignored preceding row item" (full - remaining == 50 + gap)
  _ <- frame ctx id $ do
    nextWidth 70
    _ <- textInput "short"
    _ <- textInput "default"
    row $ do
      nextWidth 90
      nextHeight 25
      scrollColumn (box 200 200)
      box 10 10
    box 8 8
  rs4 <- readRects ctx
  assert "layout: input width override was not one-shot"
    (length [r | r <- rs4, rectW r == 70] == 1 && length [r | r <- rs4, rectW r == 180] == 1)
  let viewport = [r | r <- rs4, rectW r == 90 && rectH r == 25]
  vr <- bigRect viewport
  assert "layout: scroll region broke its enclosing row"
    (Rect (rectX vr + 90 + gap) (rectY vr) 10 10 `elem` rs4
      && Rect 10 (rectY vr + 25 + gap) 8 8 `elem` rs4)

-- Spacing and indentation move the row cursor without inflating its bounds;
-- newline preserves row flow and resets the accumulated line height.
testLayoutTransitions :: IO ()
testLayoutTransitions = do
  ctx <- newTestContext
  let gap = themeGap defaultTheme
      box = image 0
  _ <- frame ctx id $ do
    row $ do
      box 10 20
      space 7
      indent 5 (box 4 8)
      newline
      box 6 3
      space 100
    box 2 2
  rs <- readRects ctx
  assert "layout: row transitions changed placement or included trailing space"
    (sortOn (\r -> (rectX r, rectY r)) rs == sortOn (\r -> (rectX r, rectY r))
      [Rect 10 10 10 20, Rect (32 + gap) 10 4 8,
       Rect 10 (30 + gap) 6 3, Rect 10 (33 + 2 * gap) 2 2])
  _ <- frame ctx id (nextWidth 25 >> nextHeight 12 >> column (pure ()) >> box 2 2)
  afterEmpty <- readRects ctx
  assert "layout: empty group failed to consume its size overrides"
    (afterEmpty == [Rect 10 (22 + gap) 2 2])

testClippedInteraction :: IO ()
testClippedInteraction = do
  ctx <- newTestContext
  let view = do
        nextWidth 100
        nextHeight 20
        result <- scrollColumn $ do
          space 30
          clicked <- button "hidden"
          value <- textInput "hidden"
          selected <- table ["header"] [["row"]]
          pure (clicked, value, selected)
        width <- availWidth
        pure (result, width)
  _ <- frame ctx id view
  rs <- readRects ctx
  let buttons = [r | r <- rs, rectY r == 40]
  b <- bigRect buttons
  _ <- frame ctx (at (rectX b + 1) (rectY b + 1) . pressLeft) view
  ((clicked, _, _), width) <- frame ctx releaseLeft view
  assert "scroll: invisible button received click" (not clicked)
  inp <- contextInput ctx
  assert "scroll: child width leaked into parent" (width == sizeW (inputWindowSize inp) - 20)
  let fields = [r | r <- rs, rectW r == 96]
  f <- bigRect fields
  _ <- frame ctx (at (rectX f + 1) (rectY f + 1) . pressLeft) view
  ((_, value, _), _) <- frame ctx (typeText "!" . releaseLeft) view
  assert "scroll: invisible field took focus" (value == "hidden")
  let tableRects = [r | r <- rs, rectH r == 44]
  t <- bigRect tableRects
  ((_, _, selected), _) <- frame ctx (at (rectX t + 1) (rectY t + 30) . pressLeft) view
  assert "scroll: invisible table row selected" (selected == Nothing)

testConstrainedPrimitives :: IO ()
testConstrainedPrimitives = do
  ctx <- newTestContext
  let check :: String -> ChibiUI () a -> IO ()
      check name view = do
        (_, dd) <- runFrame ctx emptyInput view
        r <- bigRect =<< readRects ctx
        points <- verticesFor dd (const True)
        assert (name ++ ": geometry escaped assigned rectangle") (all (inside r) points)
  check "button" (nextWidth 12 >> nextHeight 8 >> button "very long caption")
  check "table" (nextWidth 30 >> nextHeight 25 >> table ["first", "second"] [["long cell", "other"]])
  check "image" (nextWidth 30 >> nextHeight 25 >> image 0 200 200)
  check "slider" (nextWidth 40 >> nextHeight 10 >> slider 30 0 100)
  check "plot" (nextWidth 30 >> nextHeight 12 >> plotLines [0, 1, 0, -1, 0.5])
  (_, dd) <- runFrame ctx emptyInput (nextWidth 0 >> nextHeight 0 >> image 0 200 200)
  assert "draw: zero-sized image emitted degenerate geometry" (drawVertexCount dd == 0)

primaryMods :: Modifiers
primaryMods = if os == "darwin" then noModifiers {modSuper = True} else noModifiers {modCtrl = True}

chord :: Modifiers -> [Key] -> Input -> Input
chord mods ks inp = (keys ks inp) {inputModifiers = mods}

testSelectionEditing :: IO ()
testSelectionEditing = do
  ctx <- newTestContext
  clip <- newIORef Nothing
  withClipboard ctx (readIORef clip) (writeIORef clip . Just)
  let view = textInput "alpha βeta"
      primary ks = chord primaryMods ks
      plain ks = chord noModifiers ks
  _ <- frame ctx (plain [KeyTab]) view
  _ <- frame ctx (chord (noModifiers {modShift = True}) (replicate 4 KeyLeft)) view
  _ <- frame ctx (primary [KeyChar 'c']) view
  copied <- readIORef clip
  assert "selection: copy did not preserve Unicode selection" (copied == Just "βeta")
  cut <- frame ctx (primary [KeyChar 'x']) view
  assert "selection: cut removed wrong range" (cut == "alpha ")
  undone <- frame ctx (primary [KeyChar 'z']) view
  assert "selection: undo failed" (undone == "alpha βeta")
  redone <- frame ctx (chord (primaryMods {modShift = True}) [KeyChar 'z']) view
  assert "selection: redo failed" (redone == "alpha ")
  _ <- frame ctx (primary [KeyChar 'z']) view
  replaced <- frame ctx (typeText "X" . plain []) view
  assert "selection: typing did not replace restored selection" (replaced == "alpha X")
  noRedo <- frame ctx (primary [KeyChar 'y']) view
  assert "selection: new edit failed to clear redo" (noRedo == replaced)
  _ <- frame ctx (primary [KeyChar 'a']) view
  writeIORef clip (Just "one\n\ttwo\r")
  pasted <- frame ctx (primary [KeyChar 'v']) view
  assert "selection: paste failed to replace/sanitize" (pasted == "onetwo")
  _ <- frame ctx (plain [KeyEnd]) view
  writeIORef clip (Just "untouched")
  _ <- frame ctx (primary [KeyChar 'c']) view
  emptyCopy <- readIORef clip
  assert "selection: empty selection overwrote clipboard" (emptyCopy == Just "untouched")
  _ <- frame ctx (primary [KeyChar 'a']) view
  _ <- frame ctx (typeText "one two" . plain []) view
  let wordMods = if os == "darwin" then noModifiers {modAlt = True} else primaryMods
  deleted <- frame ctx (chord wordMods [KeyBackspace]) view
  assert "selection: word deletion failed" (deleted == "one ")
  -- Enter ends editing, but an unchanged supplied value retains history.
  _ <- frame ctx (plain [KeyEnter]) view
  let retained = textInput "one "
  _ <- frame ctx (plain [KeyTab]) retained
  restored <- frame ctx (primary [KeyChar 'z']) retained
  assert "selection: undo history was lost on blur" (restored == "one two")
  ctx2 <- newTestContext
  let caretView = textInput "abc"
  _ <- frame ctx2 (plain [KeyTab]) caretView
  _ <- frame ctx2 (plain [KeyHome, KeyDelete]) caretView
  _ <- frame ctx2 (primary [KeyChar 'z']) caretView
  inserted <- frame ctx2 (typeText "X" . plain []) caretView
  assert "selection: undo restored deletion range instead of original caret" (inserted == "Xabc")

testMouseSelection :: IO ()
testMouseSelection = do
  ctx <- newTestContext
  clip <- newIORef Nothing
  withClipboard ctx (readIORef clip) (writeIORef clip . Just)
  let view = do
        result <- textInput "abcd"
        a <- measureText "a"
        abc <- measureText "abc"
        pure (result, a, abc)
  (_, a, abc) <- frame ctx id view
  _ <- frame ctx (at (14 + a) 17 . pressLeft) view
  _ <- frame ctx (at (14 + abc) 17) view
  _ <- frame ctx releaseLeft view
  _ <- frame ctx (chord primaryMods [KeyChar 'c']) view
  copied <- readIORef clip
  assert "mouse selection: drag/hit testing selected wrong characters" (copied == Just "bc")
  (replaced, _, _) <- frame ctx (typeText "X" . chord noModifiers []) view
  assert "mouse selection: typing did not replace drag range" (replaced == "aXd")
  -- Selection paint and caret must remain inside a narrow field after End.
  let narrow = nextWidth 45 >> textInput "a very long line of selectable text"
  ctx2 <- newTestContext
  _ <- frame ctx2 (keys [KeyTab]) narrow
  _ <- frame ctx2 (chord primaryMods [KeyChar 'a']) narrow
  (_, dd) <- runFrame ctx2 emptyInput narrow
  points <- verticesFor dd (const True)
  r <- bigRect =<< readRects ctx2
  assert "selection: highlight/caret escaped narrow field" (all (inside r) points)

testContextMenus :: IO ()
testContextMenus = do
  ctx <- newTestContext
  clip <- newIORef Nothing
  withClipboard ctx (readIORef clip) (writeIORef clip . Just)
  let view = textInput "menu text"
      plain ks = chord noModifiers ks
  _ <- frame ctx (plain [KeyTab]) view
  _ <- frame ctx (chord primaryMods [KeyChar 'a']) view
  _ <- frame ctx (chord (noModifiers {modShift = True}) [KeyF 10]) view
  -- Undo and Redo are disabled; Cut is the first enabled menu item.
  _ <- frame ctx (plain [KeyDown]) view -- Copy
  value <- frame ctx (plain [KeyEnter]) view
  copied <- readIORef clip
  assert "menu: keyboard Copy diverged from editor command" (value == "menu text" && copied == Just value)
  _ <- frame ctx (chord (noModifiers {modShift = True}) [KeyF 10]) view
  retained <- frame ctx (plain [KeyEscape]) view
  replaced <- frame ctx (typeText "X" . plain []) view
  assert "menu: Escape leaked to field or stole its selection" (retained == "menu text" && replaced == "X")
  _ <- frame ctx (at 20 20 . applyMouseButton MouseRight True . plain []) view
  _ <- frame ctx (applyMouseButton MouseRight False) view
  blocked <- frame ctx (typeText "ignored") view
  assert "menu: typing leaked through popup" (blocked == "X")
  _ <- frame ctx (plain [KeyEscape]) view
  -- A custom popup overlays later widgets and outside dismissal cannot click
  -- the button beneath it. Actions run before the next view, with fresh ids.
  ctx2 <- newContext (Model 0 "")
  clicks <- newIORef (0 :: Int)
  let custom = do
        label "target"
        contextMenu [("Run", modify (\m -> m {modelCount = modelCount m + 1})), ("Other", pure ())]
        clicked <- button "underneath"
        when clicked (liftIO (modifyIORef' clicks (+ 1)))
        gets modelCount
  _ <- frame ctx2 id custom
  _ <- frame ctx2 (at 12 12 . applyMouseButton MouseRight True) custom
  _ <- frame ctx2 (applyMouseButton MouseRight False) custom
  -- Deferred actions must read the latest model, not an opening-time snapshot.
  runChibiUI ctx2 (put (Model 40 "latest"))
  n <- frame ctx2 (at 20 20 . pressLeft) custom
  _ <- frame ctx2 releaseLeft custom
  c <- readIORef clicks
  assert "menu: custom action used stale state or click leaked" (n == 41 && c == 0)
  text <- runChibiUI ctx2 (gets modelText)
  assert "menu: action overwrote unrelated model state" (text == "latest")
  _ <- frame ctx2 (at 12 12 . applyMouseButton MouseRight True) custom
  _ <- frame ctx2 (applyMouseButton MouseRight False) custom
  _ <- frame ctx2 (at 400 400 . pressLeft) custom
  _ <- frame ctx2 releaseLeft custom
  n' <- runChibiUI ctx2 (gets modelCount)
  assert "menu: outside dismissal invoked action" (n' == 41)
  let keyboardCustom = do
        _ <- button "target button"
        contextMenu [("Run", modify (\m -> m {modelCount = modelCount m + 1}))]
  _ <- frame ctx2 (chord noModifiers [KeyTab]) keyboardCustom
  _ <- frame ctx2 (chord (noModifiers {modShift = True}) [KeyF 10]) keyboardCustom
  _ <- frame ctx2 (chord noModifiers [KeyEnter]) keyboardCustom
  n'' <- runChibiUI ctx2 (gets modelCount)
  assert "menu: attached button did not open via Shift-F10" (n'' == 42)
  -- Near the bottom/right edge every overlay vertex remains in the window.
  let edge = do
        space 60
        indent 70 $ label "edge" >> contextMenu [("A long menu title", pure ())]
      input = emptyInput {inputWindowSize = Size 120 100}
  (_, dd) <- runFrame ctx2 (at 82 72 (applyMouseButton MouseRight True input)) edge
  points <- verticesFor dd (const True)
  assert "menu: overlay escaped window" (all (inside (Rect 0 0 120 100)) points)

testKeyboardButtons :: IO ()
testKeyboardButtons = do
  ctx <- newTestContext
  let view = do
        b <- button "press"
        field <- textInput "field"
        app <- primaryShortcut False (KeyChar 's')
        pure (b, field, app)
  (_, _, app) <- frame ctx (chord primaryMods [KeyChar 's']) view
  assert "shortcut: primary chord failed" app
  _ <- frame ctx (chord noModifiers [KeyTab]) view
  (enter, _, _) <- frame ctx (keys [KeyEnter]) view
  (spacePressed, _, _) <- frame ctx (keys [KeySpace]) view
  assert "button: Enter/Space keyboard activation failed" (enter && spacePressed)
  _ <- frame ctx (keys [KeyTab]) view
  (notButton, changed, suppressed) <- frame ctx (typeText "!" . chord noModifiers []) view
  (_, _, stillSuppressed) <- frame ctx (chord primaryMods [KeyChar 's']) view
  assert "focus: button/field routing or shortcut suppression failed"
    (not notButton && changed == "field!" && not suppressed && not stillSuppressed)

-- Focus wraps in declaration order, drops removed widgets, and gives an
-- unclaimed pointer press priority over Tab in the same frame.
testFocusTransitions :: IO ()
testFocusTransitions = do
  ctx <- newTestContext
  let view names = mapM (\name -> withKey name (button name)) names
      allButtons = view ["a", "b", "c"]
      plain = chord noModifiers
      tab = plain [KeyTab]
      back = chord (noModifiers {modShift = True}) [KeyTab]
      activate = frame ctx (plain [KeyEnter]) allButtons
  _ <- frame ctx back allButtons
  lastButton <- activate
  _ <- frame ctx tab allButtons
  firstButton <- activate
  _ <- frame ctx back allButtons
  wrappedBack <- activate
  assert "focus: Tab wrapping changed"
    (lastButton == [False, False, True] && firstButton == [True, False, False] && wrappedBack == lastButton)
  _ <- frame ctx id (view ["a", "b"])
  removed <- activate
  assert "focus: removed widget regained focus" (not (or removed))
  _ <- frame ctx (at 700 500 . pressLeft . tab) allButtons
  outside <- frame ctx (releaseLeft . plain [KeyEnter]) allButtons
  assert "focus: Tab overrode outside click" (not (or outside))
  _ <- frame ctx tab (view [])
  _ <- frame ctx back allButtons
  reentered <- activate
  assert "focus: empty order broke reentry" (reentered == lastButton)

testTextAlignment :: IO ()
testTextAlignment = do
  ctx <- newTestContext
  let glyphY align = do
        (_, dd) <- runFrame ctx emptyInput (withTextAlign align (nextHeight 53 >> label "align"))
        points <- glyphVertices dd
        pure (minimum (map snd points))
  top <- glyphY AlignTop
  middle <- glyphY AlignMiddle
  bottom <- glyphY AlignBottom
  -- Snapping rounds each line's baseline to whole device pixels, so the
  -- offsets hold to within a pixel rather than exactly.
  assert "alignment: top/middle/bottom offsets incorrect"
    (abs (middle - top - 18.5) <= 1 && abs (bottom - top - 37) <= 1)
  _ <- frame ctx id (row (alignTextToFrame >> label "caption" >> textInput "value"))
  rs <- readRects ctx
  assert "alignment: caption does not match field height" (length rs == 2 && all ((== 26) . rectH) rs)

testDpiGeometry :: IO ()
testDpiGeometry = do
  ctx <- newTestContext
  let view = do
        nextWidth 100
        nextHeight 30
        _ <- textInput "scaled"
        uiScale
  results <- mapM (\scale -> do
    setScale ctx scale
    (actual, dd) <- runFrame ctx emptyInput view
    points <- verticesFor dd (const True)
    rs <- readRects ctx
    assert "DPI: geometry escaped logical bounds" (all (inside (Rect 10 10 100 30)) points)
    pure (actual, rs)) [1, 1.25, 1.5, 2, 1]
  assert "DPI: scale changed widget geometry" (all ((== [Rect 10 10 100 30]) . snd) results)
  assert "DPI: query does not reflect scale" (map fst results == [1, 1.25, 1.5, 2, 1])
  setScale ctx (0 / 0)
  actual <- frame ctx id uiScale
  assert "DPI: invalid scale was not normalized" (actual == 1)

testSelectableText :: IO ()
testSelectableText = do
  ctx <- newTestContext
  clip <- newIORef Nothing
  withClipboard ctx (readIORef clip) (writeIORef clip . Just)
  let view = selectableText "read only"
  _ <- frame ctx (keys [KeyTab]) view
  _ <- frame ctx (chord primaryMods [KeyChar 'a']) view
  _ <- frame ctx (typeText "bad" . chord noModifiers [KeyBackspace]) view
  _ <- frame ctx (chord primaryMods [KeyChar 'x']) view
  _ <- frame ctx (chord primaryMods [KeyChar 'c']) view
  text <- readIORef clip
  assert "selectable label: edit commands changed source" (text == Just "read only")
  _ <- frame ctx (chord primaryMods [KeyChar 'a']) (selectableText "updated")
  _ <- frame ctx (chord primaryMods [KeyChar 'c']) (selectableText "updated")
  updated <- readIORef clip
  assert "selectable label: external value did not update" (updated == Just "updated")

-- Expansion runs the body immediately, and treats the whole branch as one
-- parent item without changing later siblings' identities.
testTreeInteraction :: IO ()
testTreeInteraction = do
  ctx <- newContext (0 :: Int)
  let view = do
        treeNode "Branch" (modify (+ 1) >> label "leaf")
        image 0 7 7
      step input = do
        runChibiUI ctx (put 0)
        frame ctx input view
        runChibiUI ctx get
      tailRect = do
        rs <- frameRects ctx
        case [(k, r) | (k, r) <- rs, rectW r == 7, rectH r == 7] of
          [result] -> pure result
          _ -> fail "tree: missing following sibling"
  closed <- step id
  (closedId, closedRect) <- tailRect
  pressed <- step (at 12 12 . pressLeft)
  cancelled <- step (at 500 400 . releaseLeft)
  assert "tree: body ran while closed or after cancelled click" (closed == 0 && pressed == 0 && cancelled == 0)
  _ <- step (at 12 12 . pressLeft)
  opened <- step releaseLeft
  (openId, openRect) <- tailRect
  rs <- readRects ctx
  assert "tree: click did not open in the release frame" (opened == 1)
  assert "tree: expansion changed sibling identity or failed to reserve height"
    (closedId == openId && rectY openRect > rectY closedRect && rectX openRect == rectX closedRect)
  assert "tree: child is not indented below the header"
    (any (\r -> rectH r == 16 && rectX r > 10 && rectY r > 35 && rectY r + rectH r < rectY openRect) rs)
  persisted <- step id
  left <- step (keys [KeyLeft])
  right <- step (keys [KeyRight])
  rightAgain <- step (keys [KeyRight])
  spaceClosed <- step (keys [KeySpace])
  enterOpened <- step (keys [KeyEnter])
  assert "tree: keyboard control or expansion persistence failed"
    ([persisted, left, right, rightAgain, spaceClosed, enterOpened] == [1, 0, 1, 1, 0, 1])
  _ <- step (at 12 12 . pressLeft)
  mouseClosed <- step releaseLeft
  assert "tree: second click did not collapse" (mouseClosed == 0)
  (_, restoredRect) <- tailRect
  assert "tree: collapse failed to restore layout" (restoredRect == closedRect)

testNestedTree :: IO ()
testNestedTree = do
  ctx <- newContext (Model 0 "seed")
  let view = do
        treeNode "Parent" $ treeNode "Child" $ do
          value <- textInput =<< gets modelText
          modify (\m -> m {modelText = value})
        clicked <- button "After"
        when clicked (modify (\m -> m {modelCount = modelCount m + 1}))
  -- Closed children do not appear in the Tab order.
  frame ctx (keys [KeyTab]) view
  frame ctx (keys [KeyTab]) view
  frame ctx (keys [KeyEnter]) view
  count <- runChibiUI ctx (gets modelCount)
  assert "tree: collapsed children intercepted Tab" (count == 1)
  frame ctx (keys [KeyTab]) view -- parent
  frame ctx (keys [KeyRight]) view
  frame ctx (keys [KeyTab]) view -- child
  frame ctx (keys [KeyRight]) view
  frame ctx (keys [KeyTab]) view -- text field
  frame ctx (typeText "!") view
  frame ctx (keys [KeyLeft]) view -- edit the field, not either ancestor
  edited <- runChibiUI ctx (gets modelText)
  expanded <- frameRects ctx
  assert "tree: nested field failed to edit or ancestor consumed its keys" (edited == "seed!" && length expanded == 4)
  frame ctx (at 12 12 . pressLeft) view
  frame ctx releaseLeft view
  collapsed <- frameRects ctx
  assert "tree: collapsing parent left children visible" (length collapsed == 2)
  frame ctx (keys [KeyRight]) view
  reopened <- frameRects ctx
  value <- runChibiUI ctx (gets modelText)
  assert "tree: nested expansion or model was lost on parent collapse"
    (reopened == expanded && value == "seed!")

testKeyedTree :: IO ()
testKeyedTree = do
  ctx <- newContext ([] :: [Text])
  let view order = mapM_ (\key -> withKey key (treeNode "same title" (modify (++ [key]) >> label key))) order
      step order input = do
        runChibiUI ctx (put [])
        frame ctx input (view order)
        runChibiUI ctx get
  _ <- step ["a", "b"] (keys [KeyTab])
  first <- step ["a", "b"] (keys [KeyEnter])
  reordered <- step ["b", "a"] id
  assert "tree: expansion followed position/title instead of key" (first == ["a"] && reordered == ["a"])
  -- The focused header follows its key as well.
  collapsed <- step ["b", "a"] (keys [KeyLeft])
  assert "tree: focus failed to follow keyed node" (null collapsed)

testClippedTree :: IO ()
testClippedTree = do
  ctx <- newContext (0 :: Int)
  let hidden = do
        nextWidth 100
        nextHeight 10
        scrollColumn $ do
          space 20
          treeNode "Hidden" (modify (+ 1))
  frame ctx (at 12 32 . pressLeft) hidden
  frame ctx releaseLeft hidden
  frame ctx (keys [KeyTab]) hidden
  frame ctx (keys [KeyEnter]) hidden
  count <- runChibiUI ctx get
  assert "tree: clipped header received mouse or keyboard activation" (count == 0)
  (_, dd) <- runFrame ctx emptyInput (nextWidth 8 >> treeNode "Long title" (pure ()))
  points <- verticesFor dd (const True)
  rs <- readRects ctx
  case rs of
    [r] -> assert "tree: narrow header painting escaped bounds"
      (rectW r == 8 && not (null points) && all (inside r) points)
    _ -> fail "tree: unexpected narrow header geometry"

-- | The view monad and the plumbing widgets are written with: widget ids,
-- the store, cursor placement, input reads, and drawing helpers.
--
-- A @ChibiUI model a@ runs against the context and draws straight into its draw
-- list. Widgets are ordinary calls: take an id, place a rect, read this
-- frame's input, draw, return what the user did.
module ChibiUI.Internal.Monad
  ( ChibiUI
  , mapMsg
  , mapModel
  , edit
  , runChibiUI
  , readCtx
  , writeCtx
  , liftIO
  -- * Widget identity
  , nextId
  , withKey
  -- * Widget state
  , widgetState
  , setWidgetState
  , updateWidgetState
  -- * Application model
  , get
  , gets
  , put
  , modify
  , modify'
  -- * Placement
  , place
  , layoutScope
  , readLayout
  , availWidth
  , textSize
  , paddedText
  , measureText
  , sameLine
  , newline
  , row
  , column
  , indent
  , nextWidth
  , fillWidth
  , nextHeight
  , space
  -- * Input reads
  , getInput
  , mousePos
  , mousePressed
  , mouseReleased
  , mouseHeld
  , keyPressed
  , keyHeld
  , shortcut
  , primaryShortcut
  , uiScale
  , setUiScale
  , windowSize
  , windowWidth
  , uiTime
  , scrollDelta
  -- * Interaction
  , hovered
  , claimActive
  , isActive
  , isFocused
  , requestFocus
  , blurFocus
  , addFocusable
  , wantCursor
  , recordRect
  , lookupWidgetRect
  -- * Last item
  , itemRect
  , itemHovered
  , itemFocused
  , itemActive
  , holdsWithin
  -- * Drawing
  , theme
  , withTheme
  , disabled
  , isDisabled
  , withTextAlign
  , alignTextToFrame
  , drawIO
  , withClip
  , currentClipRect
  , fillRectUI
  , strokeRectUI
  , drawTextIn
  , drawTextAt
  , textInRect
  -- * Frame control
  , requestFrame
  , requestFrameAt
  , quitUi
  , useImageRgba
  , getClipboard
  , setClipboard
  ) where

import Control.Monad.IO.Class (MonadIO (..))
import Control.Monad (when)
import Control.Monad.Reader (MonadReader (..), ReaderT (..), asks, withReaderT)
import Control.Monad.State.Class (MonadState (..), gets, modify, modify')
import qualified Data.ByteString as BS
import Data.IORef
import qualified Data.IntMap.Strict as IM
import Data.Text (Text)
import Data.Word (Word64)
import ChibiUI.Internal.Context
  ( Context (..)
  , ModelAccess (..)
  , ImageEntry (..)
  , Wake (..)
  , noWidget
  )
import ChibiUI.Internal.Draw
import ChibiUI.Internal.Font (lineHeight, fontDrawText, fontMeasure, fontScale)
import ChibiUI.Internal.Id
import ChibiUI.Internal.Input
import ChibiUI.Internal.Layout (LayoutState)
import qualified ChibiUI.Internal.Layout as Layout
import ChibiUI.Internal.RectTable (insertRect, lookupRect)
import ChibiUI.Internal.Store
import ChibiUI.Internal.Style (Theme (..), TextAlign, alignedTextY, fieldHeight, widgetPad)
import ChibiUI.Internal.Types

-- | A view, or one widget's body, with access to an application model.
-- The context retains the model between invocations.
newtype ChibiUI model a = ChibiUI (ReaderT (Context model) IO a)
  deriving newtype (Functor, Applicative, Monad, MonadIO, MonadReader (Context model))

-- | Model updates are visible immediately and persist across frames. Keeping
-- the model in the context also lets deferred menu actions use its latest value.
instance MonadState model (ChibiUI model) where
  get = ask >>= liftIO . readModel . ctxModel
  put model = state (const ((), model))
  state f = do
    ctx <- ask
    liftIO (stateModel (ctxModel ctx) f)

-- | Wrap a child view's returned message in its parent's message type.
-- This is 'fmap'; for optional messages use @mapMsg (fmap ParentMsg)@.
mapMsg :: (msg -> parentMsg) -> ChibiUI model msg -> ChibiUI model parentMsg
mapMsg = fmap

-- | Run a child view against part of the model. The setter takes the updated
-- child and the current parent. Reads and writes are live, including actions
-- retained by a context menu. Layout, focus and widget identity are shared;
-- use 'withKey' when separate children need stable identities.
--
-- > mapModel count (\n m -> m {count = n}) counter
--
-- Getter and setter should obey the usual lens laws: setting what was read
-- changes nothing, reading what was set returns it, and the latest set wins.
mapModel :: (parent -> child) -> (child -> parent -> parent) -> ChibiUI child a -> ChibiUI parent a
mapModel project replace (ChibiUI view) = ChibiUI (withReaderT focus view)
  where
    focus ctx = ctx {ctxModel = access}
      where
        parent = ctxModel ctx
        access = ModelAccess
          { readModel = project <$> readModel parent
          , stateModel = \f -> stateModel parent $ \whole ->
              let (result, child) = f (project whole) in (result, replace child whole)
          }

-- | Show part of the model in a widget and keep what the widget returns,
-- with a getter and setter as 'mapModel' takes them. Returns the new value.
--
-- > edit name (\v m -> m {name = v}) textInput
edit :: (model -> a) -> (a -> model -> model) -> (a -> ChibiUI model a) -> ChibiUI model a
edit project replace widget = do
  v <- widget =<< gets project
  modify (replace v)
  pure v

-- | Run an action against a context, retaining any model updates in it.
runChibiUI :: Context model -> ChibiUI model a -> IO a
runChibiUI ctx (ChibiUI m) = runReaderT m ctx

-- | Read one of the context's references.
{-# INLINE readCtx #-}
readCtx :: (Context model -> IORef a) -> ChibiUI model a
readCtx field = asks field >>= liftIO . readIORef

-- | Write one of the context's references.
{-# INLINE writeCtx #-}
writeCtx :: (Context model -> IORef a) -> a -> ChibiUI model ()
writeCtx field v = asks field >>= \ref -> liftIO (writeIORef ref v)

-- | Strictly modify one of the context's references.
{-# INLINE modifyCtx #-}
modifyCtx :: (Context model -> IORef a) -> (a -> a) -> ChibiUI model ()
modifyCtx field f = asks field >>= \ref -> liftIO (modifyIORef' ref f)

-- | This widget's id: the next sibling position hashed into the container's
-- path. The same widgets must run in the same order every frame, or state
-- and input follow the wrong widget.
{-# INLINE nextId #-}
nextId :: ChibiUI model WidgetId
nextId = do
  ctx <- ask
  liftIO $ do
    path <- readIORef (ctxIdPath ctx)
    sib <- readIORef (ctxIdSib ctx)
    writeIORef (ctxIdSib ctx) $! sib + 1
    pure $! widgetIdAt path sib

-- | Run a container's body: the next sibling position becomes the child
-- path, and the body's widgets count from a fresh sibling counter.
scoped :: ChibiUI model a -> ChibiUI model a
scoped = withIdScope positionalPath

-- | Run a body under a key unique among its siblings, so widgets whose
-- order changes keep their state. The same key twice in one container is
-- two widgets sharing an id.
withKey :: Text -> ChibiUI model a -> ChibiUI model a
withKey key = withIdScope (keyedPath key)

-- | Run a body in a child scope whose path @childPath@ derives from the
-- parent's path and sibling position. The scope takes the parent's next
-- sibling position, as a widget would.
withIdScope :: (Word64 -> Word64 -> Word64) -> ChibiUI model a -> ChibiUI model a
withIdScope childPath body = do
  ctx <- ask
  (path, sib) <- liftIO $ do
    path <- readIORef (ctxIdPath ctx)
    sib <- readIORef (ctxIdSib ctx)
    writeIORef (ctxIdPath ctx) $! childPath path sib
    writeIORef (ctxIdSib ctx) 0
    pure (path, sib)
  a <- body
  liftIO $ do
    writeIORef (ctxIdPath ctx) path
    writeIORef (ctxIdSib ctx) $! sib + 1
  pure a

-- | The hashed key of a widget id, as the store addresses it.
{-# INLINE slotOf #-}
slotOf :: WidgetId -> Int
slotOf = fromIntegral . hashWidgetId

-- | A widget's entry in one of the store's maps.
widgetState :: StoreMap s -> WidgetId -> ChibiUI model (Maybe s)
widgetState m wid = lookupState m (slotOf wid) <$> readCtx ctxStore

-- | Replace ('Just') or remove ('Nothing') a widget's entry. Write only a
-- changed value: each write rebuilds a path through the map.
setWidgetState :: StoreMap s -> WidgetId -> Maybe s -> ChibiUI model ()
setWidgetState m wid st = modifyCtx ctxStore (writeState m (slotOf wid) st)

-- | Move a widget's entry from @old@, as read this frame, to @new@,
-- writing only when they differ.
updateWidgetState :: Eq s => StoreMap s -> WidgetId -> Maybe s -> Maybe s -> ChibiUI model ()
updateWidgetState m wid old new = when (new /= old) (setWidgetState m wid new)

-- | Place a widget at the cursor: take the rectangle its size needs,
-- advance the cursor past it, and return the rectangle. A 'nextWidth' or
-- 'nextHeight' override replaces the measured size once.
{-# INLINE place #-}
place :: Size -> ChibiUI model Rect
place sz = do
  gap' <- themeGap <$> theme
  layoutState (Layout.placeLayout gap' sz)

-- | The effect boundary for pure cursor transitions.
layoutState :: (LayoutState -> (a, LayoutState)) -> ChibiUI model a
layoutState transition = do
  ctx <- ask
  liftIO $ do
    current <- readIORef (ctxLayout ctx)
    let (result, next) = transition current
    next `seq` writeIORef (ctxLayout ctx) next
    pure result

-- | The cursor as it stands.
readLayout :: ChibiUI model LayoutState
readLayout = readCtx ctxLayout

layoutCommand :: Layout.LayoutCommand -> ChibiUI model ()
layoutCommand command = do
  gap' <- themeGap <$> theme
  layoutState (\ls -> ((), Layout.stepLayout gap' command ls))

-- | The width a widget filling the line can take: the remaining width at
-- the cursor, accounting for siblings and indentation.
availWidth :: ChibiUI model Float
availWidth = Layout.remainingWidth <$> readLayout

-- | The width of one line of text, in logical pixels.
measureText :: Text -> ChibiUI model Float
measureText t = do
  font <- readCtx ctxFont
  liftIO (fontMeasure font t)

-- | The size of one line of text, for widgets that wrap it.
textSize :: Text -> ChibiUI model Size
textSize t = do
  w <- measureText t
  pure (Size w lineHeight)

-- | A line of text and a widget's padding around it.
paddedText :: Text -> ChibiUI model Size
paddedText t = (\(Size w h) -> Size (w + widgetPad * 2) (h + widgetPad * 2)) <$> textSize t

-- | Keep the next widget on the current line, to the right of the last one
-- placed. For gluing a pair together, or for putting a widget that a helper
-- places onto the current line; a run of widgets belongs in a 'row', whose
-- children flow left to right without repeated calls ('sameLine' inside a
-- 'row' is a no-op).
--
-- The command is one-shot and binds to the next widget /placed/, not the
-- next statement: @sameLine >> when p (label x)@ with @p@ false leaves the
-- line open for whichever widget is placed later. Guard the 'sameLine'
-- itself, or use 'row', when widgets are conditional.
sameLine :: ChibiUI model ()
sameLine = layoutCommand Layout.SameLine

-- | Step the cursor down to a fresh line.
newline :: ChibiUI model ()
newline = layoutCommand Layout.Newline

-- | Lay out a horizontal group: its children flow left to right, and the
-- group occupies one item in its parent. Preferred over repeated 'sameLine'
-- for any run of two or more widgets.
row :: ChibiUI model a -> ChibiUI model a
row = group True

-- | Lay out a vertical group, reserving its full bounds in the parent.
column :: ChibiUI model a -> ChibiUI model a
column = group False

group :: Bool -> ChibiUI model a -> ChibiUI model a
group horizontal body = do
  (a, size) <- layoutScope (Layout.beginGroup horizontal) Layout.endGroup body
  a <$ place size

-- | Indent a body's lines by @n@ logical pixels.
indent :: Float -> ChibiUI model a -> ChibiUI model a
indent n body = fst <$> layoutScope (Layout.beginIndent n) (\parent child -> ((), Layout.endIndent parent child)) body

-- | Run a body in a layout scope: @enter@ derives the body's cursor from
-- the parent's, and @leave@ gets the parent and the body's final cursor
-- back, to settle the cursor after it. Every layout scope is an id scope.
layoutScope
  :: (LayoutState -> LayoutState)
  -> (LayoutState -> LayoutState -> (b, LayoutState))
  -> ChibiUI model a
  -> ChibiUI model (a, b)
layoutScope enter leave body = do
  parent <- layoutState (\ls -> (ls, enter ls))
  a <- scoped body
  b <- layoutState (leave parent)
  pure (a, b)

-- | Give the next widget a width, instead of its measured one.
nextWidth :: Float -> ChibiUI model ()
nextWidth = layoutCommand . Layout.NextWidth

-- | Give the next widget the rest of the line's width, as a field beside
-- its caption fills out to the right edge.
fillWidth :: ChibiUI model ()
fillWidth = availWidth >>= nextWidth

-- | Give the next widget a height, instead of its measured one.
nextHeight :: Float -> ChibiUI model ()
nextHeight = layoutCommand . Layout.NextHeight

-- | Step the cursor by @n@ logical pixels: down on a fresh line, right on
-- an open one.
space :: Float -> ChibiUI model ()
space = layoutCommand . Layout.Space

-- | This frame's input.
getInput :: ChibiUI model Input
getInput = do
  ctx <- ask
  liftIO $ do
    inp <- readIORef (ctxInput ctx)
    blocked <- readIORef (ctxInputBlocked ctx)
    pure (if blocked then (clearEphemeral inp) {inputKeysHeld = [], inputButtonsHeld = noButtons} else inp)

-- | The pointer, in window coordinates.
mousePos :: ChibiUI model V2
mousePos = inputMousePos <$> getInput

-- | Whether the left button went down this frame.
mousePressed :: ChibiUI model Bool
mousePressed = pressedIn MouseLeft <$> getInput

-- | Whether the left button came up this frame.
mouseReleased :: ChibiUI model Bool
mouseReleased = releasedIn MouseLeft <$> getInput

-- | Whether the left button is down.
mouseHeld :: ChibiUI model Bool
mouseHeld = heldIn MouseLeft <$> getInput

-- | Whether a key went down this frame. A focused text field reads keys
-- for itself, so this reports 'False' while one has the keyboard.
keyPressed :: Key -> ChibiUI model Bool
keyPressed = appKey . pressedIn

-- | Whether a key is down. Also gated on typing, like 'keyPressed'.
keyHeld :: Key -> ChibiUI model Bool
keyHeld = appKey . heldIn

-- | An input test that stands down while a text field has the keyboard.
appKey :: (Input -> Bool) -> ChibiUI model Bool
appKey test = do
  typing <- readCtx ctxTyping
  (not typing &&) . test <$> getInput

-- | An application shortcut, matched exactly. Suppressed while a text
-- field is focused, so its editing shortcuts cannot trigger app actions.
shortcut :: Modifiers -> Key -> ChibiUI model Bool
shortcut mods k = appKey $ \inp -> pressedIn k inp && inputModifiers inp == mods

-- | A portable Ctrl/Command shortcut; the Bool requests Shift as well.
primaryShortcut :: Bool -> Key -> ChibiUI model Bool
primaryShortcut shift k = appKey $ \inp ->
  let m = inputModifiers inp
   in pressedIn k inp && modPrimary m && modShift m == shift && not (modAlt m)
        && not (modCtrl m && modSuper m)

-- | Current device pixels per logical UI pixel.
uiScale :: ChibiUI model Float
uiScale = readCtx ctxFont >>= liftIO . fontScale

-- | Request a device scale; zero restores automatic monitor DPI. Takes
-- effect in the next native frame. Non-finite values are ignored.
setUiScale :: Float -> ChibiUI model ()
setUiScale scale = when (isFinite scale) $ do
  writeCtx ctxScaleOverride (max 0 scale)
  requestFrame

-- | The window's size, in logical pixels.
windowSize :: ChibiUI model Size
windowSize = inputWindowSize <$> getInput

-- | The window's width, in logical pixels.
windowWidth :: ChibiUI model Float
windowWidth = sizeW <$> windowSize

-- | Monotonic seconds, for blink and other time-based looks.
uiTime :: ChibiUI model Double
uiTime = readCtx ctxTime

-- | The wheel steps this frame: x rightward, y downward.
scrollDelta :: ChibiUI model V2
scrollDelta = inputScroll <$> getInput

-- | Whether the pointer is over a rectangle, in window coordinates.
hovered :: Rect -> ChibiUI model Bool
hovered r = do
  p <- mousePos
  clip <- currentClipRect
  blocked <- readCtx ctxInputBlocked
  pure (not blocked && rectHit r p && rectHit clip p)

-- | Claim the pointer for a widget while its button is held. 'True' when
-- this call took the grab, so the widget owns the drag.
claimActive :: WidgetId -> ChibiUI model Bool
claimActive wid = do
  ctx <- ask
  liftIO $
    atomicModifyIORef' (ctxActive ctx) $ \cur ->
      if cur == noWidget
        then (wid, True)
        else (cur, cur == wid)

-- | Whether the widget owns the pointer grab.
isActive :: WidgetId -> ChibiUI model Bool
isActive wid = (== wid) <$> readCtx ctxActive

-- | Whether the widget has keyboard focus.
isFocused :: WidgetId -> ChibiUI model Bool
isFocused wid = (== wid) <$> readCtx ctxFocus

-- | Give a widget the keyboard.
requestFocus :: WidgetId -> ChibiUI model ()
requestFocus wid = do
  writeCtx ctxFocus wid
  writeCtx ctxFocusRequested True

-- | Take the keyboard from whatever widget holds it.
blurFocus :: ChibiUI model ()
blurFocus = writeCtx ctxFocus noWidget

-- | Mark a widget placed at @r@ as reachable with Tab, in declaration
-- order, while any of it shows through the clip. A widget that takes
-- typing silences app keys while focused.
addFocusable :: WidgetId -> Rect -> Bool -> ChibiUI model ()
addFocusable wid r typing = do
  ctx <- ask
  liftIO $ do
    clip <- currentClip (ctxArena ctx)
    when (rectsOverlap clip r) $
      modifyIORef' (ctxFocusables ctx) ((wid, typing) :)

-- | Ask for a pointer shape while the pointer is over this widget.
wantCursor :: UiCursorKind -> ChibiUI model ()
wantCursor = writeCtx ctxCursor

-- | Record where a widget landed this frame. Hit tests read these rects
-- directly: the cursor layout is deterministic, so a widget's rect is
-- where it was, except on the frame the layout itself changed.
{-# INLINE recordRect #-}
recordRect :: WidgetId -> Rect -> ChibiUI model ()
recordRect wid r = do
  rects <- asks ctxRects
  liftIO (insertRect rects (slotOf wid) r)

-- | Where a widget landed this frame, if it has been declared yet.
lookupWidgetRect :: WidgetId -> ChibiUI model (Maybe Rect)
lookupWidgetRect wid = do
  rects <- asks ctxRects
  liftIO (lookupRect rects (slotOf wid))

-- | The rect of the widget or group placed last.
itemRect :: ChibiUI model Rect
itemRect = Layout.lsLast <$> readLayout

-- | Whether the pointer is over the widget or group placed last.
itemHovered :: ChibiUI model Bool
itemHovered = itemRect >>= hovered

-- | Whether the focused widget lies within the widget or group placed last.
itemFocused :: ChibiUI model Bool
itemFocused = itemRect >>= (`holdsWithin` ctxFocus)

-- | Whether the widget holding the pointer grab lies within the widget or
-- group placed last: it is being pressed or dragged.
itemActive :: ChibiUI model Bool
itemActive = itemRect >>= (`holdsWithin` ctxActive)

-- | Whether the widget one of the context's references names lies within
-- a rect.
holdsWithin :: Rect -> (Context model -> IORef WidgetId) -> ChibiUI model Bool
holdsWithin r field = do
  wid <- readCtx field
  if wid == noWidget then pure False else maybe False (rectContains r) <$> lookupWidgetRect wid

-- | The theme, for colours and spacing.
theme :: ChibiUI model Theme
theme = readCtx ctxTheme

-- | Draw a body with another theme.
withTheme :: Theme -> ChibiUI model a -> ChibiUI model a
withTheme t = locally ctxTheme (const t)

-- | Run a body with its widgets disabled when the flag is 'True': they
-- draw dimmed, and ignore the pointer and keyboard, so a disabled button
-- never clicks and Tab skips its widgets. Scroll regions still scroll.
disabled :: Bool -> ChibiUI model a -> ChibiUI model a
disabled False body = body
disabled True body = locally ctxDisabled (const True) (locally ctxTheme dim body)
  where
    dim th = th {themeText = themeTextDim th, themeAccent = themeBorder th}

-- | Whether widgets declared here are 'disabled'.
isDisabled :: ChibiUI model Bool
isDisabled = readCtx ctxDisabled

-- | Draw a body with text aligned vertically another way.
withTextAlign :: TextAlign -> ChibiUI model a -> ChibiUI model a
withTextAlign align = locally ctxTheme (\th -> th {themeTextAlign = align})

-- | Run a body with one of the context's references changed, restoring it
-- after.
locally :: (Context model -> IORef s) -> (s -> s) -> ChibiUI model a -> ChibiUI model a
locally field f body = do
  old <- readCtx field
  writeCtx field (f old)
  body <* writeCtx field old

-- | Give the next label the standard field height, aligning captions with
-- their neighboring text/number input in a row.
alignTextToFrame :: ChibiUI model ()
alignTextToFrame = nextHeight fieldHeight

-- | Emit into the draw list, under the current clip.
{-# INLINE drawIO #-}
drawIO :: (DrawArena -> IO ()) -> ChibiUI model ()
drawIO f = asks ctxArena >>= liftIO . f

-- | Restrict both painting and pointer hit tests to a fixed rectangle.
withClip :: Rect -> ChibiUI model a -> ChibiUI model a
withClip r body = do
  ctx <- ask
  liftIO (pushClip (ctxArena ctx) r)
  a <- body
  liftIO (popClip (ctxArena ctx))
  pure a

-- | The clip as it stands, in window coordinates.
currentClipRect :: ChibiUI model Rect
currentClipRect = asks ctxArena >>= liftIO . currentClip

-- | A solid rectangle, in window coordinates.
{-# INLINE fillRectUI #-}
fillRectUI :: Rect -> Color -> ChibiUI model ()
fillRectUI r c = drawIO $ \a -> fillRect a r c

-- | A border of @bw@ logical pixels drawn inside a rectangle.
{-# INLINE strokeRectUI #-}
strokeRectUI :: Rect -> Float -> Color -> ChibiUI model ()
strokeRectUI r bw c = drawIO $ \a -> strokeRect a r bw c

-- | One line of text clipped to a rectangle, with its top-left at the
-- rectangle's top-left.
drawTextIn :: Rect -> Text -> Color -> ChibiUI model ()
drawTextIn = textAlignedIn 0

-- | Centre one line of text in a rectangle.
textInRect :: Rect -> Text -> Color -> ChibiUI model ()
textInRect = textAlignedIn 0.5

-- | One line of text clipped to a rectangle: @ax@ of the way across it,
-- as 'drawTextAt' aligns, and placed vertically as the theme aligns text.
textAlignedIn :: Float -> Rect -> Text -> Color -> ChibiUI model ()
textAlignedIn ax r t col = do
  th <- theme
  withClip r $ drawTextAt (V2 (rectX r + rectW r * ax) (alignedTextY (themeTextAlign th) r)) ax 0 t col

-- | One line of text at a point: @ax@ and @ay@ in 0..1 name the alignment
-- point within the text's box, so @(0, 0.5)@ centres on the point
-- vertically. Clipped to the current clip only.
drawTextAt :: V2 -> Float -> Float -> Text -> Color -> ChibiUI model ()
drawTextAt (V2 x y) ax ay t col = do
  w <- if ax == 0 then pure 0 else measureText t
  drawGlyphs (x - w * ax) (y - lineHeight * ay) t col

-- All text paths share atlas selection and quad emission. A line that
-- lies clear of the clip, by a line height of margin for glyphs that
-- overhang their box, emits nothing and skips the glyph walk.
drawGlyphs :: Float -> Float -> Text -> Color -> ChibiUI model ()
drawGlyphs x y t col = do
  ctx <- ask
  liftIO $ do
    let a = ctxArena ctx
    Rect cx cy cw ch <- currentClip a
    when (cw > 0 && ch > 0 && x < cx + cw + lineHeight
      && y < cy + ch + lineHeight && y + lineHeight * 2 > cy) $ do
      font <- readIORef (ctxFont ctx)
      fontDrawText font a x y col t

-- | Ask for another frame even without input, as a view that changes on a
-- timer does.
requestFrame :: ChibiUI model ()
requestFrame = modifyCtx ctxWake (min WakeSoon)

-- | Ask for a frame once 'uiTime' reaches @t@, as a blink or a timeout
-- does; the loop sleeps until then unless input comes first. Non-finite
-- times are ignored.
requestFrameAt :: Double -> ChibiUI model ()
requestFrameAt t = when (isFinite t) (modifyCtx ctxWake (min (WakeAt t)))

-- | End the session after this frame.
quitUi :: ChibiUI model ()
quitUi = writeCtx ctxQuit True

-- | Register an RGBA image under an id: @w@ x @h@ pixels, top row first,
-- four bytes per pixel. Call it every frame the image shows; the backend
-- uploads it when @version@ changes. Draw it with 'ChibiUI.image'.
useImageRgba :: Int -> Int -> Int -> Int -> BS.ByteString -> ChibiUI model ()
useImageRgba img w h version px = modifyCtx ctxImages (IM.insert img (ImageEntry w h version px))

-- | The system clipboard's text, if it holds any.
getClipboard :: ChibiUI model (Maybe Text)
getClipboard = readCtx ctxClipboardGet >>= liftIO

-- | Replace the system clipboard's text.
setClipboard :: Text -> ChibiUI model ()
setClipboard t = readCtx ctxClipboardPut >>= \write -> liftIO (write $! t)

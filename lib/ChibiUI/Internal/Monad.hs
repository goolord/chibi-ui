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
  , runChibiUI
  , askContext
  , liftIO
  -- * Widget identity
  , nextId
  , scoped
  , withKey
  -- * Store
  , storeModify
  , storeRead
  , slotOf
  -- * Application model
  , get
  , gets
  , put
  , modify
  , modify'
  -- * Placement
  , place
  , availableWidth
  , textSize
  , measureText
  , sameLine
  , newline
  , row
  , column
  , indent
  , nextWidth
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
  , typedText
  -- * Interaction
  , hovered
  , claimActive
  , isActive
  , dropActive
  , isFocused
  , requestFocus
  , blurFocus
  , addFocusable
  , wantCursor
  , recordRect
  , prevRect
  -- * Drawing
  , theme
  , withTheme
  , withTextAlign
  , alignTextToFrame
  , drawIO
  , withClip
  , fillRectUI
  , strokeRectUI
  , drawTextIn
  , drawTextAt
  , textInRect
  , drawImageUV
  -- * Frame control
  , requestFrame
  , quitUi
  , registerImage
  , getClipboard
  , setClipboard
  ) where

import Control.Monad.IO.Class (MonadIO (..))
import Control.Monad (when)
import Control.Monad.Reader (MonadReader (..), ReaderT (..), withReaderT)
import Control.Monad.State.Class (MonadState (..), gets, modify, modify')
import Data.Bits (xor)
import qualified Data.ByteString as BS
import Data.IORef
import qualified Data.IntMap.Strict as IM
import Data.Text (Text)
import qualified Data.Text as T
import Data.Word (Word64)
import ChibiUI.Internal.Context
  ( Context (..)
  , ModelAccess (..)
  , ImageEntry (..)
  , noWidget
  , readClipboard
  , writeClipboard
  )
import ChibiUI.Internal.Draw
import ChibiUI.Internal.Font (lineHeight, fontDrawText, fontMeasure)
import ChibiUI.Internal.Id
import ChibiUI.Internal.Input
import ChibiUI.Internal.Layout (LayoutState)
import qualified ChibiUI.Internal.Layout as Layout
import ChibiUI.Internal.RectTable (insertRect, lookupRect)
import ChibiUI.Internal.Store
import ChibiUI.Internal.Style (Theme (..), TextAlign, alignedTextY, fieldPad)
import ChibiUI.Internal.Types

-- | A view, or one widget's body, with access to an application model.
-- The context retains the model between invocations.
newtype ChibiUI model a = ChibiUI (ReaderT (Context model) IO a)
  deriving newtype (Functor, Applicative, Monad, MonadIO, MonadReader (Context model))

-- | Model updates are visible immediately and persist across frames. Keeping
-- the model in the context also lets deferred menu actions use its latest value.
instance MonadState model (ChibiUI model) where
  get = askContext >>= liftIO . readModel . ctxModel
  put model = do
    ctx <- askContext
    liftIO (writeModel (ctxModel ctx) model)
  state f = do
    ctx <- askContext
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
          , writeModel = \child -> stateModel parent (\whole -> ((), replace child whole))
          , stateModel = \f -> stateModel parent $ \whole ->
              let (result, child) = f (project whole) in (result, replace child whole)
          }

-- | Run an action against a context, retaining any model updates in it.
runChibiUI :: Context model -> ChibiUI model a -> IO a
runChibiUI ctx (ChibiUI m) = runReaderT m ctx

-- | The context, for the plumbing that needs more than one helper.
askContext :: ChibiUI model (Context model)
askContext = ask

-- | This widget's id: the next sibling position hashed into the container's
-- path. The same widgets must run in the same order every frame, or state
-- and input follow the wrong widget.
{-# INLINE nextId #-}
nextId :: ChibiUI model WidgetId
nextId = do
  ctx <- ask
  liftIO $ do
    cid <- readIORef (ctxIdPath ctx)
    sib <- readIORef (ctxIdSib ctx)
    writeIORef (ctxIdSib ctx) (sib + 1)
    let raw = mix64 cid sib
    pure (if raw == 0 then WidgetId 1 else WidgetId raw)

-- | Run a container's body: the next sibling position becomes the child
-- path, and the body's widgets count from a fresh sibling counter.
scoped :: ChibiUI model a -> ChibiUI model a
scoped = withIdScope (enterScope scopeTag)

-- | Run a body under a key unique among its siblings, so widgets whose
-- order changes keep their state. The same key twice in one container is
-- two widgets sharing an id.
withKey :: Text -> ChibiUI model a -> ChibiUI model a
withKey key = withIdScope (enterKeyed (fnv1a (T.unpack key)))

withIdScope :: (IdContext -> (IdContext, IdContext)) -> ChibiUI model a -> ChibiUI model a
withIdScope enter body = do
  ctx <- ask
  parent' <- liftIO $ do
    parent <- IdContext <$> readIORef (ctxIdPath ctx) <*> readIORef (ctxIdSib ctx)
    let (parent', child) = enter parent
    writeIORef (ctxIdPath ctx) (currentId child)
    writeIORef (ctxIdSib ctx) (siblingId child)
    pure parent'
  a <- body
  liftIO $ do
    writeIORef (ctxIdPath ctx) (currentId parent')
    writeIORef (ctxIdSib ctx) (siblingId parent')
  pure a

fnv1a :: String -> Word64
fnv1a =
  foldl'
    (\acc c -> ((fromIntegral (fromEnum c) :: Word64) `xor` acc) * 0x00000100000001B3)
    0xcbf29ce484222325

-- | The hashed key of a widget id, as the store addresses it.
{-# INLINE slotOf #-}
slotOf :: WidgetId -> Int
slotOf = fromIntegral . hashWidgetId

-- | Read and write the store in one go.
storeModify :: (WidgetStore -> (a, WidgetStore)) -> ChibiUI model a
storeModify f = do
  ctx <- ask
  liftIO $ do
    st <- readIORef (ctxStore ctx)
    let (a, st') = f st
    writeIORef (ctxStore ctx) st'
    pure a

-- | Read the store.
storeRead :: (WidgetStore -> a) -> ChibiUI model a
storeRead f = do
  ctx <- ask
  liftIO (f <$> readIORef (ctxStore ctx))

-- | Place a widget at the cursor: take the rectangle its size needs,
-- advance the cursor past it, and return the rectangle. A 'nextWidth' or
-- 'nextHeight' override replaces the measured size once.
{-# INLINE place #-}
place :: Size -> ChibiUI model Rect
place sz = do
  gap' <- themeGap <$> theme
  layoutState (Layout.placeLayout gap' sz)

-- The effect boundary for pure cursor transitions.
layoutState :: (LayoutState -> (a, LayoutState)) -> ChibiUI model a
layoutState transition = do
  ctx <- ask
  liftIO $ do
    current <- readIORef (ctxLayout ctx)
    let (result, next) = transition current
    next `seq` writeIORef (ctxLayout ctx) next
    pure result

layoutCommand :: Layout.LayoutCommand -> ChibiUI model ()
layoutCommand command = do
  gap' <- themeGap <$> theme
  layoutState (\ls -> ((), Layout.stepLayout gap' command ls))

-- | Remaining width at the cursor, accounting for siblings and indentation.
availableWidth :: ChibiUI model Float
availableWidth = do
  ctx <- ask
  liftIO (Layout.remainingWidth <$> readIORef (ctxLayout ctx))

-- | The width of one line of text, in logical pixels.
measureText :: Text -> ChibiUI model Float
measureText t = do
  ctx <- ask
  font <- liftIO (readIORef (ctxFont ctx))
  liftIO (fontMeasure font t)

-- | The size of one line of text, for widgets that wrap it.
textSize :: Text -> ChibiUI model Size
textSize t = do
  w <- measureText t
  pure (Size w lineHeight)

-- | Keep the next widget on the current line, to the right of the last one
-- placed. Only the next item stays on that line; a 'row' keeps all its
-- children horizontal without repeated calls.
sameLine :: ChibiUI model ()
sameLine = layoutCommand Layout.SameLine

-- | Step the cursor down to a fresh line.
newline :: ChibiUI model ()
newline = layoutCommand Layout.Newline

-- | Lay out a horizontal group. The group occupies one item in its parent.
row :: ChibiUI model a -> ChibiUI model a
row = group True

-- | Lay out a vertical group, reserving its full bounds in the parent.
column :: ChibiUI model a -> ChibiUI model a
column = group False

group :: Bool -> ChibiUI model a -> ChibiUI model a
group horizontal body = do
  before <- layoutState (\ls -> (ls, Layout.beginGroup horizontal ls))
  a <- scoped body
  let origin = V2 (Layout.lsPenX before) (Layout.lsLineY before)
  size <- layoutState (\after -> (Layout.contentSize origin after, before))
  _ <- place size
  pure a

-- | Indent a body's lines by @n@ logical pixels.
indent :: Float -> ChibiUI model a -> ChibiUI model a
indent n body = do
  before <- layoutState (\ls -> (ls, Layout.beginIndent n ls))
  a <- scoped body
  layoutState (\after -> ((), Layout.endIndent before after))
  pure a

-- | Give the next widget a width, instead of its measured one.
nextWidth :: Float -> ChibiUI model ()
nextWidth = layoutCommand . Layout.NextWidth

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

-- | Whether a key went down this frame. A focused field reads keys for
-- itself, so this reports 'False' while anything has the keyboard.
keyPressed :: Key -> ChibiUI model Bool
keyPressed k = do
  inp <- getInput
  focus <- readFocus
  pure (focus == noWidget && pressedIn k inp)

-- | Whether a key is down. Also gated on focus, like 'keyPressed'.
keyHeld :: Key -> ChibiUI model Bool
keyHeld k = do
  inp <- getInput
  focus <- readFocus
  pure (focus == noWidget && heldIn k inp)

-- | An application shortcut, matched exactly. Suppressed while a widget
-- has focus so its editing/navigation shortcuts cannot trigger app actions.
shortcut :: Modifiers -> Key -> ChibiUI model Bool
shortcut mods k = do
  inp <- getInput
  pressed <- keyPressed k
  pure (pressed && inputModifiers inp == mods)

-- | A portable Ctrl/Command shortcut; the Bool requests Shift as well.
primaryShortcut :: Bool -> Key -> ChibiUI model Bool
primaryShortcut shift k = do
  inp <- getInput
  pressed <- keyPressed k
  let m = inputModifiers inp
  pure (pressed && modPrimary m && modShift m == shift && not (modAlt m)
    && not (modCtrl m && modSuper m))

-- | Current device pixels per logical UI pixel.
uiScale :: ChibiUI model Float
uiScale = askContext >>= liftIO . readIORef . ctxScale

-- | Request a device scale; zero restores automatic monitor DPI. Takes
-- effect in the next native frame. Non-finite values are ignored.
setUiScale :: Float -> ChibiUI model ()
setUiScale scale = when (not (isNaN scale || isInfinite scale)) $ do
  ctx <- askContext
  liftIO (writeIORef (ctxScaleOverride ctx) (max 0 scale))
  requestFrame

-- | The text typed this frame, for fields.
typedText :: ChibiUI model Text
typedText = T.pack . inputChars <$> getInput

-- | The window's size, in logical pixels.
windowSize :: ChibiUI model Size
windowSize = inputWindowSize <$> getInput

-- | The window's width, in logical pixels.
windowWidth :: ChibiUI model Float
windowWidth = sizeW <$> windowSize

-- | Monotonic seconds, for blink and other time-based looks.
uiTime :: ChibiUI model Double
uiTime = do
  ctx <- ask
  liftIO (readIORef (ctxTime ctx))

-- | The wheel steps this frame: x rightward, y downward.
scrollDelta :: ChibiUI model V2
scrollDelta = inputScroll <$> getInput

readFocus :: ChibiUI model WidgetId
readFocus = do
  ctx <- ask
  liftIO (readIORef (ctxFocus ctx))

-- | Whether the pointer is over a rectangle, in window coordinates.
hovered :: Rect -> ChibiUI model Bool
hovered r = do
  p <- mousePos
  ctx <- ask
  clip <- liftIO (currentClip (ctxArena ctx))
  blocked <- liftIO (readIORef (ctxInputBlocked ctx))
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
isActive wid = do
  ctx <- ask
  liftIO ((== wid) <$> readIORef (ctxActive ctx))

-- | Release the pointer grab.
dropActive :: ChibiUI model ()
dropActive = do
  ctx <- ask
  liftIO (writeIORef (ctxActive ctx) noWidget)

-- | Whether the widget has keyboard focus.
isFocused :: WidgetId -> ChibiUI model Bool
isFocused wid = (== wid) <$> readFocus

-- | Give a widget the keyboard.
requestFocus :: WidgetId -> ChibiUI model ()
requestFocus wid = do
  ctx <- ask
  liftIO $ do
    writeIORef (ctxFocus ctx) wid
    writeIORef (ctxFocusRequested ctx) True

-- | Take the keyboard from whatever widget holds it.
blurFocus :: ChibiUI model ()
blurFocus = do
  ctx <- ask
  liftIO (writeIORef (ctxFocus ctx) noWidget)

-- | Mark a widget as reachable with Tab, in declaration order.
addFocusable :: WidgetId -> ChibiUI model ()
addFocusable wid = do
  ctx <- ask
  liftIO $ do
    rects <- readIORef (ctxRects ctx)
    rect <- lookupRect rects (slotOf wid)
    clip <- currentClip (ctxArena ctx)
    when (maybe False (maybe False (const True) . rectIntersect clip) rect) $
      modifyIORef' (ctxFocusables ctx) (wid :)

-- | Ask for a pointer shape while the pointer is over this widget.
wantCursor :: UiCursorKind -> ChibiUI model ()
wantCursor k = do
  ctx <- ask
  liftIO (writeIORef (ctxCursor ctx) k)

-- | Record where a widget landed this frame. Hit tests read these rects
-- directly: the cursor layout is deterministic, so a widget's rect is
-- where it was, except on the frame the layout itself changed.
{-# INLINE recordRect #-}
recordRect :: WidgetId -> Rect -> ChibiUI model ()
recordRect wid r = do
  ctx <- ask
  liftIO $ do
    rects <- readIORef (ctxRects ctx)
    insertRect rects (slotOf wid) r

-- | Where the widget landed last frame, for hit tests that must survive
-- the frame a layout change happens in.
prevRect :: WidgetId -> ChibiUI model (Maybe Rect)
prevRect wid = do
  ctx <- ask
  liftIO $ do
    prev <- readIORef (ctxPrevRects ctx)
    lookupRect prev (slotOf wid)

-- | The theme, for colours and spacing.
theme :: ChibiUI model Theme
theme = do
  ctx <- ask
  liftIO (readIORef (ctxTheme ctx))

-- | Draw a body with another theme.
withTheme :: Theme -> ChibiUI model a -> ChibiUI model a
withTheme t body = do
  ctx <- ask
  old <- liftIO (readIORef (ctxTheme ctx))
  liftIO (writeIORef (ctxTheme ctx) t)
  a <- body
  liftIO (writeIORef (ctxTheme ctx) old)
  pure a

withTextAlign :: TextAlign -> ChibiUI model a -> ChibiUI model a
withTextAlign align body = do
  th <- theme
  withTheme th {themeTextAlign = align} body

-- | Give the next label the standard field height, aligning captions with
-- their neighboring text/number input in a row.
alignTextToFrame :: ChibiUI model ()
alignTextToFrame = nextHeight (lineHeight + fieldPad * 2 + 2)

-- | Emit into the draw list. The arena's clip and texture are whatever the
-- caller left them as; save and restore if that matters.
{-# INLINE drawIO #-}
drawIO :: (DrawArena -> IO ()) -> ChibiUI model ()
drawIO f = do
  ctx <- ask
  liftIO (f (ctxArena ctx))

-- | Restrict both painting and pointer hit tests to a fixed rectangle.
withClip :: Rect -> ChibiUI model a -> ChibiUI model a
withClip r body = do
  ctx <- ask
  liftIO (pushClip (ctxArena ctx) r)
  a <- body
  liftIO (popClip (ctxArena ctx))
  pure a

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
drawTextIn r t col = do
  th <- theme
  withClip r $ drawGlyphs (rectX r) (alignedTextY (themeTextAlign th) r) t col

-- | One line of text at a point: @ax@ and @ay@ in 0..1 name the alignment
-- point within the text's box, so @(0, 0.5)@ centres on the point
-- vertically. Clipped to the current clip only.
drawTextAt :: V2 -> Float -> Float -> Text -> Color -> ChibiUI model ()
drawTextAt (V2 x y) ax ay t col = do
  w <- measureText t
  drawGlyphs (x - w * ax) (y - lineHeight * ay) t col

-- All text paths share atlas selection and quad emission.
drawGlyphs :: Float -> Float -> Text -> Color -> ChibiUI model ()
drawGlyphs x y t col = do
  ctx <- ask
  liftIO $ do
    let a = ctxArena ctx
    font <- readIORef (ctxFont ctx)
    setTexture a texGlyphAtlas
    fontDrawText font a x y col t
    setTexture a texFlat

-- | Centre one line of text in a rectangle.
textInRect :: Rect -> Text -> Color -> ChibiUI model ()
textInRect r t col = do
  th <- theme
  withClip r $ drawTextAt
    (V2 (rectX r + rectW r / 2) (alignedTextY (themeTextAlign th) r))
    0.5 0 t col

-- | An image the backend registered, drawn at a size. Texture id 2 or more.
drawImageUV :: Int -> Rect -> ChibiUI model ()
drawImageUV img r = drawIO $ \a -> do
  setTexture a (texImage img)
  emitQuadUV a (rectX r) (rectY r) (rectX r + rectW r) (rectY r + rectH r) colorWhite 0 0 1 1
  setTexture a texFlat

-- | Ask for another frame even without input, as a view that changes on a
-- timer does.
requestFrame :: ChibiUI model ()
requestFrame = do
  ctx <- ask
  liftIO (writeIORef (ctxFrameRequest ctx) True)

-- | End the session after this frame.
quitUi :: ChibiUI model ()
quitUi = do
  ctx <- ask
  liftIO (writeIORef (ctxQuit ctx) True)

-- | Register an RGBA image under an id, @w@ x @h@ pixels, top row first.
-- Call it every frame the image shows; the backend uploads it when the
-- version changes.
registerImage :: Int -> Int -> Int -> Int -> BS.ByteString -> ChibiUI model ()
registerImage img w h ver px = do
  ctx <- ask
  liftIO (modifyIORef' (ctxImages ctx) (IM.insert img (ImageEntry w h ver px)))

-- | The system clipboard's text, if it holds any.
getClipboard :: ChibiUI model (Maybe Text)
getClipboard = do
  ctx <- ask
  liftIO (readClipboard ctx)

-- | Replace the system clipboard's text.
setClipboard :: Text -> ChibiUI model ()
setClipboard t = do
  ctx <- ask
  liftIO (writeClipboard ctx t)

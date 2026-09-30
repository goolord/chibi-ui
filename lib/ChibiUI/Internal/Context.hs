-- | The context: the state that outlives a frame. One per window; the
-- backend creates it, folds each frame's events into its input, runs the
-- view against it, and renders the draw list it leaves behind.
module ChibiUI.Internal.Context
  ( Context (..)
  , ModelAccess (..)
  , ImageEntry (..)
  , Popup (..)
  , newContext
  , noWidget
  , setTheme
  , setScale
  , withClipboard
  , setFont
  , contextInput
  , frameRects
  , readClipboard
  , writeClipboard
  ) where

import Control.Monad (join)
import Data.IntMap.Strict (IntMap)
import Data.IORef
import qualified Data.ByteString as BS
import Data.Text (Text)
import Data.Word (Word64)
import ChibiUI.Internal.Draw (DrawArena, newDrawArena)
import ChibiUI.Internal.Font (Font, embeddedFont, fontSetScale, newFont)
import ChibiUI.Internal.Id (WidgetId (..), initialIdPath)
import ChibiUI.Internal.Input (Input, UiCursorKind (..), emptyInput)
import ChibiUI.Internal.Layout (LayoutState, freshLayout)
import ChibiUI.Internal.RectTable (RectTable, newRectTable, rectTableToList)
import ChibiUI.Internal.Store (WidgetStore, emptyWidgetStore)
import ChibiUI.Internal.Style (Theme, defaultTheme)
import ChibiUI.Internal.Types (Rect, V2, validScale)

-- | One lightweight overlay. Actions run before the next view invocation.
data Popup = Popup
  { popupOwner :: !WidgetId
  , popupPosition :: !V2
  , popupItems :: ![(Text, Bool, IO ())]
  , popupSelected :: !Int
  , popupPointer :: !V2
  }

-- | An RGBA image a view registered this frame: @ieWidth@ x @ieHeight@
-- pixels, top row first, four bytes per pixel. The backend uploads an entry
-- when its version changes and binds it wherever a draw command names it.
data ImageEntry = ImageEntry
  { ieWidth :: !Int
  , ieHeight :: !Int
  , ieVersion :: !Int
  , iePixels :: !BS.ByteString
  }

-- | State that outlives a frame.
data Context model = Context
  { ctxModel :: !(ModelAccess model)
  -- ^ Application-owned state, shared by views and deferred menu actions.
  , ctxFont :: !(IORef Font)
  -- ^ The rasterized font; a scale change rebuilds it, and its atlas is
  -- synced by the backend after frames that added glyphs.
  , ctxScale :: !(IORef Float)
  , ctxScaleOverride :: !(IORef Float)
  -- ^ Zero follows the monitor; positive values override device scale.
  , ctxPopup :: !(IORef (Maybe Popup))
  , ctxInputBlocked :: !(IORef Bool)
  , ctxTheme :: !(IORef Theme)
  , ctxInput :: !(IORef Input)
  , ctxStore :: !(IORef WidgetStore)
  , ctxRects :: !(IORef RectTable)
  -- ^ This frame's widget rects, keyed by hashed id, for hit tests. The
  -- frame start clears it.
  , ctxFocus :: !(IORef WidgetId)
  , ctxFocusRequested :: !(IORef Bool)
  -- ^ Whether a widget claimed focus this frame; a click that claims none
  -- clears it.
  , ctxFocusables :: !(IORef [(WidgetId, Bool)])
  -- ^ This frame's focusable widget ids, in declaration order (reversed
  -- while building), for Tab, each with whether it takes typing.
  , ctxTyping :: !(IORef Bool)
  -- ^ Whether the focused widget takes typing, so app keys stand down.
  -- Settled after each frame's focus resolves.
  , ctxActive :: !(IORef WidgetId)
  -- ^ The widget that grabbed the pointer and has not released it.
  , ctxCursor :: !(IORef UiCursorKind)
  -- ^ The pointer shape the frame wants; widgets raise it when hovered or
  -- focused.
  , ctxTime :: !(IORef Double)
  -- ^ Monotonic seconds, for the caret blink.
  , ctxQuit :: !(IORef Bool)
  , ctxFrameRequest :: !(IORef Bool)
  -- ^ A view asked for another frame; otherwise the loop blocks on input.
  , ctxWakeAt :: !(IORef Double)
  -- ^ The earliest 'ctxTime' a view asked for a frame at; infinity for none.
  , ctxArena :: !DrawArena
  , ctxLayout :: !(IORef LayoutState)
  , ctxIdPath :: !(IORef Word64)
  -- ^ The current scope's path hash. Split from 'ctxIdSib' into two
  -- references, so handing out an id advances one counter without
  -- allocating a context record per widget.
  , ctxIdSib :: !(IORef Word64)
  -- ^ The next sibling's position within the current scope.
  , ctxImages :: !(IORef (IntMap ImageEntry))
  , ctxClipboardGet :: !(IORef (IO (Maybe Text)))
  , ctxClipboardPut :: !(IORef (Text -> IO ()))
  }

-- | Live model operations. Projections compose these rather than copying a
-- model into a temporary reference, so deferred actions see current state.
data ModelAccess model = ModelAccess
  { readModel :: IO model
  , writeModel :: model -> IO ()
  , stateModel :: forall a. (model -> (a, model)) -> IO a
  }

-- | The id no widget has.
noWidget :: WidgetId
noWidget = WidgetId 0

-- | A brand-new context: the embedded font at scale 1, the dark theme, and
-- a clipboard that holds nothing, seeded with the application's model.
newContext :: model -> IO (Context model)
newContext initial = do
  modelRef <- newIORef initial
  let model = ModelAccess (readIORef modelRef) (writeIORef modelRef) $ \f ->
        atomicModifyIORef' modelRef $ \current ->
          let (result, next) = f current in (next, result)
  font0 <- newFont embeddedFont
  font <- newIORef font0
  scale <- newIORef 1.0
  scaleOverride <- newIORef 0
  popup <- newIORef Nothing
  blocked <- newIORef False
  theme <- newIORef defaultTheme
  input <- newIORef emptyInput
  store <- newIORef emptyWidgetStore
  rects <- newRectTable >>= newIORef
  focus <- newIORef noWidget
  focusReq <- newIORef False
  focusables <- newIORef []
  typing <- newIORef False
  active <- newIORef noWidget
  cursor <- newIORef UiCursorDefault
  time <- newIORef 0
  quit <- newIORef False
  frameReq <- newIORef False
  wakeAt <- newIORef (1 / 0)
  arena <- newDrawArena
  layout <- newIORef freshLayout
  idPath <- newIORef initialIdPath
  idSib <- newIORef 0
  images <- newIORef mempty
  clipGet <- newIORef (pure Nothing)
  clipPut <- newIORef (\_ -> pure ())
  pure
    Context
      { ctxModel = model
       , ctxFont = font
       , ctxScale = scale
       , ctxScaleOverride = scaleOverride
       , ctxPopup = popup
       , ctxInputBlocked = blocked
      , ctxTheme = theme
      , ctxInput = input
      , ctxStore = store
      , ctxRects = rects
      , ctxFocus = focus
      , ctxFocusRequested = focusReq
      , ctxFocusables = focusables
      , ctxTyping = typing
      , ctxActive = active
      , ctxCursor = cursor
      , ctxTime = time
      , ctxQuit = quit
      , ctxFrameRequest = frameReq
      , ctxWakeAt = wakeAt

       , ctxArena = arena
       , ctxLayout = layout
       , ctxIdPath = idPath
       , ctxIdSib = idSib
       , ctxImages = images
      , ctxClipboardGet = clipGet
      , ctxClipboardPut = clipPut
      }

-- | Replace the theme. The next frame draws with it.
setTheme :: Context model -> Theme -> IO ()
setTheme ctx = writeIORef (ctxTheme ctx)

-- | Set the UI scale, in device pixels per logical pixel. The font
-- rebuilds at the new raster size; its atlas is re-uploaded by the backend
-- on the next sync.
setScale :: Context model -> Float -> IO ()
setScale ctx scale = do
  let valid = if validScale scale then scale else 1
  writeIORef (ctxScale ctx) valid
  font <- readIORef (ctxFont ctx)
  fontSetScale font valid

-- | Replace the font (for hosts that load a TTF of their own).
setFont :: Context model -> Font -> IO ()
setFont ctx font = do
  writeIORef (ctxFont ctx) font
  scale <- readIORef (ctxScale ctx)
  fontSetScale font scale

-- | Install the platform clipboard.
withClipboard :: Context model -> IO (Maybe Text) -> (Text -> IO ()) -> IO ()
withClipboard ctx get put = do
  writeIORef (ctxClipboardGet ctx) get
  writeIORef (ctxClipboardPut ctx) put

contextInput :: Context model -> IO Input
contextInput = readIORef . ctxInput

-- | Recorded widget geometry for hosts and tests.
frameRects :: Context model -> IO [(Int, Rect)]
frameRects ctx = readIORef (ctxRects ctx) >>= rectTableToList

-- | The system clipboard's text, if it holds any.
readClipboard :: Context model -> IO (Maybe Text)
readClipboard ctx = join (readIORef (ctxClipboardGet ctx))

-- | Replace the system clipboard's text.
writeClipboard :: Context model -> Text -> IO ()
writeClipboard ctx t = join (flip ($!) t <$> readIORef (ctxClipboardPut ctx))

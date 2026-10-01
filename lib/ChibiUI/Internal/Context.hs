{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}

-- | The context: the state that outlives a frame. One per window; the
-- backend creates it, folds each frame's events into its input, runs the
-- view against it, and renders the draw list it leaves behind.
module ChibiUI.Internal.Context
  ( Context (..)
  , ModelAccess (..)
  , ImageEntry (..)
  , Popup (..)
  , Wake (..)
  , newContext
  , noWidget
  , setTheme
  , setScale
  , withClipboard
  , setFont
  , contextInput
  , frameRects
  ) where

import Data.IntMap.Strict (IntMap)
import Data.IORef
import qualified Data.ByteString as BS
import Data.Text (Text)
import Data.Word (Word64)
import ChibiUI.Internal.Draw (DrawArena, newDrawArena)
import ChibiUI.Internal.Font (Font, embeddedFont, fontScale, fontSetScale, newFont)
import ChibiUI.Internal.Id (WidgetId (..), initialIdPath)
import ChibiUI.Internal.Input (Input, UiCursorKind (..), emptyInput)
import ChibiUI.Internal.Layout (LayoutState, freshLayout)
import ChibiUI.Internal.RectTable (RectTable, newRectTable, rectTableToList)
import ChibiUI.Internal.Store (WidgetStore, emptyWidgetStore)
import ChibiUI.Internal.Style (Theme, defaultTheme)
import ChibiUI.Internal.Types (Rect, V2)

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
  -- synced by the backend after frames that added glyphs. Its scale is
  -- the UI scale.
  , ctxScaleOverride :: !(IORef Float)
  -- ^ Zero follows the monitor; positive values override device scale.
  , ctxPopup :: !(IORef (Maybe Popup))
  , ctxTooltip :: !(IORef (Maybe (V2, Text)))
  -- ^ The tooltip to paint over this frame, and the pointer it follows.
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
  , ctxWake :: !(IORef Wake)
  -- ^ When the view wants its next frame; otherwise the loop blocks on input.
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

-- | When a view wants its next frame. Requests combine with 'min': the
-- soonest wins.
data Wake
  = WakeSoon
  -- ^ As soon as the loop's pace allows.
  | WakeAt !Double
  -- ^ Once 'ctxTime' reaches this time.
  | WakeIdle
  -- ^ Not until input arrives.
  deriving (Eq, Ord, Show)

-- | Live model operations. Projections compose these rather than copying a
-- model into a temporary reference, so deferred actions see current state.
-- Every write goes through 'stateModel', which forces the new model.
data ModelAccess model = ModelAccess
  { readModel :: IO model
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
  let ctxModel = ModelAccess (readIORef modelRef) $ \f ->
        atomicModifyIORef' modelRef $ \current ->
          let (result, next) = f current in (next, result)
  ctxFont <- newIORef =<< newFont embeddedFont
  ctxScaleOverride <- newIORef 0
  ctxPopup <- newIORef Nothing
  ctxTooltip <- newIORef Nothing
  ctxInputBlocked <- newIORef False
  ctxTheme <- newIORef defaultTheme
  ctxInput <- newIORef emptyInput
  ctxStore <- newIORef emptyWidgetStore
  ctxRects <- newIORef =<< newRectTable
  ctxFocus <- newIORef noWidget
  ctxFocusRequested <- newIORef False
  ctxFocusables <- newIORef []
  ctxTyping <- newIORef False
  ctxActive <- newIORef noWidget
  ctxCursor <- newIORef UiCursorDefault
  ctxTime <- newIORef 0
  ctxQuit <- newIORef False
  ctxWake <- newIORef WakeIdle
  ctxArena <- newDrawArena
  ctxLayout <- newIORef freshLayout
  ctxIdPath <- newIORef initialIdPath
  ctxIdSib <- newIORef 0
  ctxImages <- newIORef mempty
  ctxClipboardGet <- newIORef (pure Nothing)
  ctxClipboardPut <- newIORef (\_ -> pure ())
  pure Context {..}

-- | Replace the theme. The next frame draws with it.
setTheme :: Context model -> Theme -> IO ()
setTheme ctx = writeIORef (ctxTheme ctx)

-- | Set the UI scale, in device pixels per logical pixel. The font
-- rebuilds at the new raster size; its atlas is re-uploaded by the backend
-- on the next sync.
setScale :: Context model -> Float -> IO ()
setScale ctx scale = readIORef (ctxFont ctx) >>= (`fontSetScale` scale)

-- | Replace the font (for hosts that load a TTF of their own), at the
-- scale the current one has.
setFont :: Context model -> Font -> IO ()
setFont ctx font = do
  scale <- readIORef (ctxFont ctx) >>= fontScale
  writeIORef (ctxFont ctx) font
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

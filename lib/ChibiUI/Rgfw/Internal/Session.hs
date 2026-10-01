-- | The RGFW window session: options, the runner, and translation of RGFW
-- events into 'Input', adapted from nano-ui-rgfw.
--
-- The loop is simple: wait for events (or a short timeout when a view
-- asked for a frame, or until the time a view asked for one at), fold the
-- event batch into one 'Input', run the view, and render. Rendering
-- carries rudimentary damage tracking: the frame's draw list is diffed
-- against the last one, so a frame that changed nothing presents without
-- drawing and one that changed a little repaints only those rectangles.
module ChibiUI.Rgfw.Internal.Session
  ( RgfwOptions (..)
  , defaultRgfwOptions
  , WindowSettings (..)
  , defaultWindowSettings
  , runChibiApp
  ) where

import Control.Concurrent (rtsSupportsBoundThreads, runInBoundThread)
import Control.Exception (bracket)
import Control.Monad (forM_, unless, void)
import Data.Bits ((.&.), (.|.))
import Data.Char (chr, isPrint, toLower)
import Data.IORef
import Data.Maybe (listToMaybe)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSC
import qualified Data.Text as T
import Data.Word (Word8, Word32)
import Foreign.ForeignPtr (withForeignPtr)
import Foreign.Ptr (castPtr, nullPtr, plusPtr)
import Foreign.Storable (peekByteOff)
import GHC.Clock (getMonotonicTime)
import qualified System.Environment
import ChibiUI.Internal.Draw (DrawCmd (..), DrawData (..), quadBytes, texAtlas)
import ChibiUI.Internal.Context
  ( Context (..)
  , Wake (..)
  , newContext
  , setFont
  , setScale
  , setTheme
  , withClipboard
  )
import ChibiUI.Internal.Frame (runFrame)
import ChibiUI.Internal.Input
import ChibiUI.Internal.Monad (ChibiUI)
import ChibiUI.Internal.Font (Font, atlasSize, fontAtlasPixels, fontFree, fontScale, newFont)
import ChibiUI.Internal.Style (Theme, defaultTheme, themeWindow)
import ChibiUI.Internal.Types (Size (..), V2 (..), v2Sub, validScale)
import ChibiUI.Rgfw.Internal.Gl (GlRenderer, freeGlRenderer, newGlRenderer, readRetainedPixels, renderFrameGl)
import qualified RGFW as R

-- | Window settings for the RGFW runner. Sizes are in logical pixels.
data WindowSettings = WindowSettings
  { wsTitle :: !T.Text
  , wsSize :: !(Int, Int)
  , wsResizable :: !Bool
  , wsFullscreen :: !Bool
  }
  deriving (Eq, Show)

-- | An 800x600 resizable window titled "chibi-ui".
defaultWindowSettings :: WindowSettings
defaultWindowSettings = WindowSettings "chibi-ui" (800, 600) True False

-- | Window and rendering options for the RGFW runner.
data RgfwOptions = RgfwOptions
  { optWindow :: !WindowSettings
  , optTheme :: !Theme
  , optScale :: !Float
  -- ^ UI scale, in device pixels per logical pixel. @0@ follows the
  -- monitor's scale.
  , optRefreshHz :: !Int
  -- ^ Wake pace for a view that asks for frames; @0@ means 30 wakes per
  -- second at most.
  , optFontPath :: !(Maybe FilePath)
  -- ^ A TrueType font to rasterize instead of the embedded Inter subset.
  }

-- | The default window with the dark theme, the monitor's scale, and the
-- embedded font.
defaultRgfwOptions :: RgfwOptions
defaultRgfwOptions =
  RgfwOptions
    { optWindow = defaultWindowSettings
    , optTheme = defaultTheme
    , optScale = 0.0
    , optRefreshHz = 0
    , optFontPath = Nothing
    }

-- | Run a view with an initial application model in an owned RGFW/OpenGL
-- window, with @opts@' window, theme, and scale, until the view quits or
-- the window closes. Model updates persist across frames. Native resources
-- are released on exit; window creation failure prints a message and
-- returns.
runChibiApp :: RgfwOptions -> model -> ChibiUI model () -> IO ()
runChibiApp opts initial view = inBoundThread $
  bracket create (mapM_ R.closeWindow) $ \mWin -> case mWin of
    Nothing -> putStrLn "Failed to create RGFW window with an OpenGL 3.2 context."
    Just win -> R.withEventBuffer $ \evPtr -> runWindow win evPtr
  where
    settings = optWindow opts
    -- The GL context is current only on the OS thread that created it.
    inBoundThread act = if rtsSupportsBoundThreads then runInBoundThread act else act
    create = do
      let flags =
            (if wsResizable settings then 0 else R.rgfw_windowNoResize)
              .|. (if wsFullscreen settings then R.rgfw_windowFullscreen else 0)
          (w, h) = wsSize settings
      R.createWindowGL (T.unpack (wsTitle settings)) 0 0 w h flags 3 2
    runWindow win evPtr = do
      let !refreshHz = if optRefreshHz opts > 0 then optRefreshHz opts else 30
          !timeoutMs = max 1 (floor (1000 / fromIntegral refreshHz :: Double)) :: Int
      monScale0 <- R.windowScale win
      let !scaleInit = resolveScale (optScale opts) monScale0
      ctx <- newContext initial
      -- The font: the context's embedded subset, or a TTF of the host's
      -- choosing in its place.
      forM_ (optFontPath opts) $ \p -> do
        embedded <- readIORef (ctxFont ctx)
        BS.readFile p >>= newFont >>= setFont ctx
        fontFree embedded
      font <- readIORef (ctxFont ctx)
      setTheme ctx (optTheme opts)
      setScale ctx scaleInit
      writeIORef (ctxScaleOverride ctx) (optScale opts)
      withClipboard ctx R.readClipboardText (\t -> void (R.writeClipboardText t))
      -- Open at the chosen scale: resize from the settings' logical size,
      -- then centre the window on its monitor.
      let (lw, lh) = wsSize settings
          phys :: Int -> Int
          phys v = max 1 (round (fromIntegral v * realToFrac scaleInit :: Double))
      unless (wsFullscreen settings || (phys lw, phys lh) == (lw, lh)) $
        R.resizeWindow win (phys lw) (phys lh)
      R.centerWindow win
      R.showWindow win
      pendingRef <- newIORef (Pending emptyInput Nothing)
      cursorRef <- newIORef UiCursorDefault
      now0 <- getMonotonicTime
      lastFrameRef <- newIORef now0
      let syncCursor = do
            want <- readIORef (ctxCursor ctx)
            syncCursorKind cursorRef (R.showMouse win) setIcon want
          setIcon kind = void $ case kind of
            UiCursorPointer -> R.setMouseStandard win R.rgfw_mousePointingHand
            UiCursorText -> R.setMouseStandard win R.rgfw_mouseIbeam
            _ -> R.setMouseDefault win
          -- Fold the queued events into the input; 'True' when the window
          -- was asked to close.
          drainEvents closed = do
            ev <- R.pollEvent win evPtr
            case ev of
              R.EventNone -> pure closed
              R.EventWindowClose -> drainEvents True
              _ -> modifyIORef' pendingRef (`stepEvent` ev) >> drainEvents closed
          -- Settle the scale and the window's size and focus into the input.
          syncWindow = do
            size <- R.windowSize win
            monScale <- R.windowScale win
            userScale <- readIORef (ctxScaleOverride ctx)
            let scale = resolveScale userScale monScale
            setScale ctx scale
            focused <- R.windowFocused win
            modifyIORef' pendingRef (settleInput scale size focused)
          renderAndSwap renderer = do
            inp0 <- pendInput <$> readIORef pendingRef
            scale <- fontScale font
            (pw, ph) <- R.windowSize win
            theme1 <- readIORef (ctxTheme ctx)
            (_, dd) <- runFrame ctx inp0 view
            images <- readIORef (ctxImages ctx)
            renderFrameGl renderer font images scale (max 1 pw) (max 1 ph) (themeWindow theme1) (inputWindowSize inp0) dd
            R.swapBuffersGL win
            -- Clear one-shot events and stamp the timing of the frame that
            -- just ran onto the next one.
            now <- getMonotonicTime
            prev <- readIORef lastFrameRef
            modifyIORef' pendingRef $ \p ->
              p {pendInput = (clearEphemeral (pendInput p)) {inputDeltaTime = realToFrac (now - prev)}}
            writeIORef lastFrameRef now
            syncCursor
          loop renderer = do
            quit <- readIORef (ctxQuit ctx)
            wake <- readIORef (ctxWake ctx)
            now <- getMonotonicTime
            let wait = case wake of
                  WakeSoon -> timeoutMs
                  WakeAt t -> max 1 (ceiling ((t - now) * 1000))
                  WakeIdle -> -1
            unless quit $ do
              R.waitForEvent wait
              closed <- drainEvents False
              syncWindow
              renderAndSwap renderer
              unless closed (loop renderer)
      bracket newGlRenderer freeGlRenderer $ \renderer -> do
        -- Present the opening frame before blocking on events.
        syncWindow
        renderAndSwap renderer
        -- Debug dump of the frame and the font atlas, then quit.
        dumpPath <- System.Environment.lookupEnv "CHIBI_UI_DUMP"
        case dumpPath of
          Just path -> do
            (pw, ph) <- R.windowSize win
            -- Re-run the frame for its draw list: the arena was reused.
            (_, dd) <- readIORef pendingRef >>= \p -> runFrame ctx (pendInput p) view
            dumpFrame renderer font dd path (max 1 pw) (max 1 ph)
          Nothing -> loop renderer
    resolveScale user mon
      | validScale user = user
      | validScale mon = mon
      | otherwise = 1

-- | Write the retained frame to @path.ppm@ and print the atlas ink, the
-- frame's draw-list stats, and its first glyph quad's position and UV.
dumpFrame :: GlRenderer -> Font -> DrawData -> FilePath -> Int -> Int -> IO ()
dumpFrame renderer font dd path w h = do
  pixels <- readRetainedPixels renderer w h
  let row = w * 4
      flipped =
        BS.concat
          [BS.take row (BS.drop ((h - 1 - y) * row) pixels) | y <- [0 .. h - 1]]
      header =
        BSC.pack ("P6\n" ++ show w ++ " " ++ show h ++ "\n255\n")
      (aw, ah) = atlasSize
  ptr <- fontAtlasPixels font
  (nz, maxX, maxY) <-
    if ptr == nullPtr
      then pure (0 :: Int, 0 :: Int, 0 :: Int)
      else do
        bytes <- BS.packCStringLen (castPtr ptr, aw * ah)
        let ink = BS.elemIndices 0xff bytes
            maxX = maximum (0 : [i `mod` aw | i <- ink])
            maxY = maximum (0 : [i `div` aw | i <- ink])
        pure (BS.length (BS.filter (/= 0) bytes), maxX, maxY)
  -- P6 stores RGB, while the retained framebuffer is RGBA.
  let rgb = BS.pack [b | (i, b) <- zip [0 :: Int ..] (BS.unpack flipped), i `mod` 4 /= 3]
  BS.writeFile (path ++ ".ppm") (header <> rgb)
  let atlasCmds = [c | c <- drawCommands dd, cmdTextureId c == texAtlas]
  -- The first glyph quad's position and UV, against the atlas ink.
  uvDump <- case listToMaybe atlasCmds of
    Nothing -> pure ("no atlas cmd" :: String)
    Just c -> withForeignPtr (drawVertices dd) $ \vp -> do
      let p = castPtr vp `plusPtr` (fromIntegral (cmdFirstQuad c) * quadBytes)
      x <- peekByteOff p 0 :: IO Float
      y <- peekByteOff p 4 :: IO Float
      u <- peekByteOff p 24 :: IO Float
      v <- peekByteOff p 28 :: IO Float
      pure ("first glyph vertex x=" ++ show x ++ " y=" ++ show y ++ " u=" ++ show u ++ " v=" ++ show v)
  putStrLn
    ( "dump: " ++ path ++ ".ppm " ++ show (w, h)
        ++ " atlas " ++ show (aw, ah)
        ++ " nonzero=" ++ show nz
        ++ " inkMax(x,y)=" ++ show (maxX, maxY)
        ++ " frame: verts=" ++ show (drawVertexCount dd)
        ++ " cmds=" ++ show (length (drawCommands dd))
        ++ " atlasCmds=" ++ show (length atlasCmds)
        ++ " | " ++ uvDump
    )

-- | The key for an RGFW key code and its modifier bits.
mapRgfwKey :: Word32 -> Word8 -> Maybe Key
mapRgfwKey k m
  | Just named <- lookup k namedRgfwKeys = Just named
  | Just c <- lookup k keypadRgfwKeys = keypadKey (m .&. R.rgfw_modNumLock /= 0) c
  | k >= R.rgfw_keyF1 && k <= R.rgfw_keyF24 = Just (KeyF (fromIntegral (k - R.rgfw_keyF1) + 1))
  -- Codes below 128 are the ASCII characters the keys type unshifted on a US
  -- layout.
  | k > 32 && k < 127 = Just (KeyChar (toLower (chr (fromIntegral k))))
  | otherwise = Nothing

namedRgfwKeys :: [(Word32, Key)]
namedRgfwKeys =
  [ (R.rgfw_keyEscape, KeyEscape)
  , (R.rgfw_keyReturn, KeyEnter)
  , (R.rgfw_keyPadReturn, KeyEnter)
  , (R.rgfw_keyTab, KeyTab)
  , (R.rgfw_keyBackSpace, KeyBackspace)
  , (R.rgfw_keyDelete, KeyDelete)
  , (R.rgfw_keyLeft, KeyLeft)
  , (R.rgfw_keyRight, KeyRight)
  , (R.rgfw_keyUp, KeyUp)
  , (R.rgfw_keyDown, KeyDown)
  , (R.rgfw_keyHome, KeyHome)
  , (R.rgfw_keyEnd, KeyEnd)
  , (R.rgfw_keyPageUp, KeyPageUp)
  , (R.rgfw_keyPageDown, KeyPageDown)
  , (R.rgfw_keyInsert, KeyInsert)
  , (R.rgfw_keySpace, KeySpace)
  ]

-- | Keypad digit and point codes, by the character each types ('keypadKey').
keypadRgfwKeys :: [(Word32, Char)]
keypadRgfwKeys =
  zip
    [R.rgfw_keyPad0, R.rgfw_keyPad1, R.rgfw_keyPad2, R.rgfw_keyPad3, R.rgfw_keyPad4, R.rgfw_keyPad5, R.rgfw_keyPad6, R.rgfw_keyPad7, R.rgfw_keyPad8, R.rgfw_keyPad9, R.rgfw_keyPadPeriod]
    "0123456789."

modsFromRgfw :: Word8 -> Modifiers
modsFromRgfw m = modifiersFromBits m R.rgfw_modShift R.rgfw_modControl R.rgfw_modAlt R.rgfw_modSuper

-- | What the event queue has told the session between frames: the input,
-- which persists from frame to frame with one-shot events cleared, and the
-- pointer in device pixels, or 'Nothing' outside the window. The pointer
-- becomes logical each frame, at whatever scale that frame has.
data Pending = Pending
  { pendInput :: !Input
  , pendPointer :: !(Maybe V2)
  }

-- | Fold one RGFW event into what is pending; the loop handles close.
stepEvent :: Pending -> R.Event -> Pending
stepEvent p = \case
  R.EventMouseMotion x y -> p {pendPointer = Just (V2 (fromIntegral x) (fromIntegral y))}
  R.EventOther t | t == R.rgfw_mouseLeave -> p {pendPointer = Nothing}
  ev -> p {pendInput = applyEvent (pendInput p) ev}

-- | Settle the pointer, and the window's native size and focus, into the
-- input, in logical pixels at @scale@.
settleInput :: Float -> (Int, Int) -> Bool -> Pending -> Pending
settleInput scale (pw, ph) focused p = p {pendInput = placed {inputWindowSize = Size (logical pw) (logical ph), inputWindowFocused = focused}}
  where
    logical :: Int -> Float
    logical v = fromIntegral (max 1 (round (fromIntegral v / realToFrac scale :: Double) :: Int))
    placed = case pendPointer p of
      Just (V2 x y) -> (pendInput p) {inputMousePos = V2 (x / scale) (y / scale)}
      Nothing -> applyPointerLeave (pendInput p)

-- | Fold one RGFW event into frame input, past the pointer. Control
-- characters are dropped: some platforms send Ctrl+letter as one, and the
-- key event already reports the chord.
applyEvent :: Input -> R.Event -> Input
applyEvent inp = \case
  -- RGFW numbers buttons from 0 in 'mouseButtonNumber' order: left, middle,
  -- right, back, forward, then the rest.
  R.EventMouseButton btn down -> applyMouseButton (mouseButtonNumber (fromIntegral btn + 1)) down inp
  -- RGFW's wheel is positive up and left; the input's is down and right.
  R.EventMouseScroll dx dy -> inp {inputScroll = inputScroll inp `v2Sub` V2 dx dy}
  R.EventKeyChar ch | isPrint ch -> inp {inputChars = inputChars inp ++ [ch]}
  R.EventKeyPress k m -> key k m True
  R.EventKeyRepeat k m -> key k m True
  R.EventKeyRelease k m -> key k m False
  R.EventOther t | t == R.rgfw_windowFocusOut -> releaseAllKeys inp
  _ -> inp
  where
    -- 'applyKey' detects auto-repeat because the key is already held.
    key k m down = (maybe inp (\k' -> applyKey k' down inp) (mapRgfwKey k m)) {inputModifiers = modsFromRgfw m}

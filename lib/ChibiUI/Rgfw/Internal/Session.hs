-- | The RGFW window session: options, the runner, and translation of RGFW
-- events into 'Input', adapted from nano-ui-rgfw.
--
-- The loop is simple: wait for events (or a short timeout while a field is
-- focused, for the caret blink, or when a view asked for a frame), fold
-- the event batch into one 'Input', run the view, and render. Rendering
-- carries rudimentary damage tracking: the frame's draw list is diffed
-- against the last one, so a frame that changed nothing presents without
-- drawing and one that changed a little repaints only those rectangles.
module ChibiUI.Rgfw.Internal.Session
  ( RgfwOptions (..)
  , defaultRgfwOptions
  , WindowSettings (..)
  , defaultWindowSettings
  , runChibiAppWith
  -- * Input translation
  , RgfwEvent (..)
  , decodeRgfwEvents
  , applyRgfwEvent
  , mapRgfwCursor
  ) where

import Control.Concurrent (rtsSupportsBoundThreads, runInBoundThread)
import Control.Exception (bracket)
import Control.Monad (unless, void, when)
import Data.Bits ((.&.), (.|.))
import Data.Char (chr, isPrint, toLower)
import Data.IORef
import Data.Maybe (fromMaybe, isJust, listToMaybe, mapMaybe)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 ()
import qualified Data.Text as T
import Data.Word (Word8, Word32)
import Foreign.ForeignPtr (withForeignPtr)
import Foreign.Ptr (Ptr, castPtr, nullPtr, plusPtr)
import Foreign.Storable (peekByteOff)
import GHC.Clock (getMonotonicTime)
import qualified System.Environment
import ChibiUI.Internal.Draw (DrawCmd (..), DrawData (..), cmdTextureId, drawCommands, drawVertexCount, texGlyphAtlas)
import ChibiUI.Internal.Context
  ( Context (..)
  , newContext
  , noWidget
  , setFont
  , setScale
  , setTheme
  , withClipboard
  )
import ChibiUI.Internal.Damage (Damage (..), trackFrame)
import ChibiUI.Internal.Frame (runFrame)
import ChibiUI.Internal.Input
import ChibiUI.Internal.Monad (ChibiUI)
import ChibiUI.Internal.Font (embeddedFont, newFont)
import ChibiUI.Internal.Style (Theme, defaultTheme, themeWindow)
import ChibiUI.Internal.Types (Size (..), V2 (..))
import ChibiUI.Internal.Font (fontAtlasPixels, fontAtlasSize)
import ChibiUI.Rgfw.Internal.Gl (freeGlRenderer, newGlRenderer, readRetainedPixels, renderFrameGl, uploadImagesGl)
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
  -- ^ Wake pace for a view that asks for frames or blinks a caret; @0@
  -- means 30 wakes per second at most.
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

-- | Run a view in an owned RGFW/OpenGL window until the view quits or the
-- window closes. Native resources are released on exit; window creation
-- failure prints a message and returns.
runChibiAppWith :: RgfwOptions -> model -> ChibiUI model () -> IO ()
runChibiAppWith opts initial view = inBoundThread $
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
      let monScaleInit = if monScale0 > 0 then monScale0 else 1.0
          !scaleInit = resolveScale (optScale opts) monScaleInit
      ctx <- newContext initial
      -- The font: a TTF of the host's choosing, or the embedded subset.
      fontBytes <- case optFontPath opts of
        Just p -> BS.readFile p
        Nothing -> pure embeddedFont
      font <- newFont fontBytes
      setFont ctx font
      setTheme ctx (optTheme opts)
      setScale ctx scaleInit
      writeIORef (ctxScaleOverride ctx) (optScale opts)
      withClipboard ctx R.readClipboardText (\t -> void (R.writeClipboardText t))
      -- Open at the chosen scale: resize from the settings' logical size,
      -- then centre the window on its monitor.
      let physAt :: Float -> (Int, Int) -> (Int, Int)
          physAt scale (w, h) =
            ( max 1 (round (fromIntegral w * realToFrac scale :: Double))
            , max 1 (round (fromIntegral h * realToFrac scale :: Double))
            )
          (lw, lh) = wsSize settings
      when (not (wsFullscreen settings)) $ do
        let (rw, rh) = physAt scaleInit (lw, lh)
        when ((rw, rh) /= (lw, lh)) $ uncurry (R.resizeWindow win) (rw, rh)
      R.centerWindow win
      R.showWindow win
      -- Persistent input: each frame starts from the last, with one-shot
      -- events cleared.
      inputRef <- newIORef emptyInput
      scaleRef <- newIORef scaleInit
      cursorRef <- newIORef UiCursorDefault
      now0 <- getMonotonicTime
      lastFrameRef <- newIORef now0
      statsRef <- newIORef (0 :: Int, 0 :: Int, 0 :: Int)
      snapRef <- newIORef Nothing
      let syncCursor = do
            want <- readIORef (ctxCursor ctx)
            syncCursorKind cursorRef (R.showMouse win) (setIcon . mapRgfwCursor) want
          setIcon icon =
            void $
              if icon == R.rgfw_mouseArrow
                then R.setMouseDefault win
                else R.setMouseStandard win icon
          drainEvents = do
            scale <- readIORef scaleRef
            evs <- pollRgfwEvents win evPtr scale
            modifyIORef' inputRef (\inp -> foldl' applyRgfwEvent inp evs)
            pure (RgfwEvClose `elem` evs)
          syncWindow = do
            (pw, ph) <- R.windowSize win
            monScale <- R.windowScale win
            userScale <- readIORef (ctxScaleOverride ctx)
            oldScale <- readIORef scaleRef
            let s = resolveScale userScale monScale
            when (s /= oldScale) $ do
              writeIORef scaleRef s
              setScale ctx s
              -- Events drained above used the old scale. Rebase even when
              -- the pointer did not move during this monitor/zoom change.
              modifyIORef' inputRef $ \inp ->
                let V2 x y = inputMousePos inp
                 in inp {inputMousePos = V2 (x * oldScale / s) (y * oldScale / s)}
            scale <- readIORef scaleRef
            let logical :: Int -> Float
                logical v =
                  fromIntegral
                    (max 1 (round (fromIntegral v / realToFrac scale :: Double) :: Int))
                logicalSize = Size (logical pw) (logical ph)
            modifyIORef' inputRef (\i -> i {inputWindowSize = logicalSize})
          renderAndSwap renderer = do
            inp0 <- readIORef inputRef
            scale <- readIORef scaleRef
            (pw, ph) <- R.windowSize win
            theme1 <- readIORef (ctxTheme ctx)
            (_, dd) <- runFrame ctx inp0 view
            images <- readIORef (ctxImages ctx)
            imagesChanged <- uploadImagesGl renderer images
            damage0 <- trackFrame snapRef (inputWindowSize inp0) dd
            -- New texture contents repaint the same quads differently.
            let damage = if imagesChanged then DamageFull else damage0
            writeIORef
              statsRef
              ( drawVertexCount dd
              , length (drawCommands dd)
              , length [() | c <- drawCommands dd, cmdTextureId c == texGlyphAtlas]
              )
            renderFrameGl renderer font scale (max 1 pw) (max 1 ph) (themeWindow theme1) dd damage
            R.swapBuffersGL win
            -- Clear one-shot events and stamp the timing of the frame that
            -- just ran onto the next one.
            now <- getMonotonicTime
            prev <- readIORef lastFrameRef
            modifyIORef'
              inputRef
              ( \i ->
                  (clearEphemeral i) {inputDeltaTime = realToFrac (now - prev)}
              )
            writeIORef lastFrameRef now
            syncCursor
          loop renderer = do
            quit <- readIORef (ctxQuit ctx)
            requested <- readIORef (ctxFrameRequest ctx)
            focus <- readIORef (ctxFocus ctx)
            winFocused <- R.windowFocused win
            let blinkDue = requested || (winFocused && focus /= noWidget)
            unless quit $ do
              R.waitForEvent (if blinkDue then timeoutMs else -1)
              closed <- drainEvents
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
            let w = max 1 pw
                h = max 1 ph
            pixels <- readRetainedPixels renderer w h
            let row = w * 4
                flipped =
                  BS.concat
                    [BS.take row (BS.drop ((h - 1 - y) * row) pixels) | y <- [0 .. h - 1]]
                header =
                  BS.pack (map (fromIntegral . fromEnum) ("P6\n" ++ show w ++ " " ++ show h ++ "\n255\n"))
            (aw, ah) <- fontAtlasSize font
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
            (sv, sc, sg) <- readIORef statsRef
            -- The first glyph quad's position and UV, against the atlas ink.
            inpDump <- readIORef inputRef
            (_, ddx) <- runFrame ctx inpDump view
            let firstGlyph = listToMaybe [c | c <- drawCommands ddx, cmdTextureId c == texGlyphAtlas]
            uvDump <- case firstGlyph of
              Nothing -> pure ("no glyph cmd" :: String)
              Just c -> withForeignPtr (drawVertices ddx) $ \vp -> do
                let v0 = fromIntegral (cmdIndexOffset c) :: Int
                    p = castPtr vp `plusPtr` (v0 * 32)
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
                  ++ " frame: verts=" ++ show sv
                  ++ " cmds=" ++ show sc
                  ++ " glyphCmds=" ++ show sg
                  ++ " | " ++ uvDump
              )
          Nothing -> pure ()
        unless (isJust dumpPath) (loop renderer)
    resolveScale user mon = fromMaybe 1 (foldr (\x acc -> if x > 0 && not (isNaN x || isInfinite x) then Just x else acc) Nothing [user, mon])

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

-- | The RGFW standard cursor for a cursor kind, after 'cursorFallback'.
-- 'R.rgfw_mouseArrow' stands for the platform's default arrow.
mapRgfwCursor :: UiCursorKind -> Word8
mapRgfwCursor kind = case cursorFallback kind of
  UiCursorPointer -> R.rgfw_mousePointingHand
  UiCursorText -> R.rgfw_mouseIbeam
  _ -> R.rgfw_mouseArrow

-- | An RGFW event translated for the input fold.
data RgfwEvent
  = RgfwEvClose
  | RgfwEvMotion !Float !Float
  | RgfwEvButton !Word8 !Bool
  | RgfwEvLeave -- ^ the pointer left the window
  | RgfwEvScroll !Float !Float
  | RgfwEvChar !Char -- ^ typed character
  | RgfwEvKey !Word32 !Word8 !Bool -- ^ key, modifiers, down (including repeats) or up
  | RgfwEvFocusLost -- ^ the window lost keyboard focus
  deriving (Eq, Show)

-- | Drain the RGFW queue and decode it at a scale.
pollRgfwEvents :: R.Window -> Ptr R.RGFW_event -> Float -> IO [RgfwEvent]
pollRgfwEvents win evPtr scale = drain []
  where
    drain acc = do
      ev <- R.pollEvent win evPtr
      case ev of
        R.EventNone -> pure (decodeRgfwEvents scale (reverse acc))
        _ -> drain (ev : acc)

-- | Translate a batch of raw events in queue order. Pointer positions are
-- divided by the logical scale. Control characters are dropped: some
-- platforms send Ctrl+letter as one, and the key event already reports the
-- chord.
decodeRgfwEvents :: Float -> [R.Event] -> [RgfwEvent]
decodeRgfwEvents scale = mapMaybe $ \case
  R.EventWindowClose -> Just RgfwEvClose
  R.EventMouseMotion x y -> Just (RgfwEvMotion (fromIntegral x / scale) (fromIntegral y / scale))
  R.EventMouseButton btn down -> Just (RgfwEvButton btn down)
  -- RGFW's wheel is positive up and left; the input's is down and right.
  R.EventMouseScroll dx dy -> Just (RgfwEvScroll (negate dx) (negate dy))
  R.EventOther t
    | t == R.rgfw_mouseLeave -> Just RgfwEvLeave
    | t == R.rgfw_windowFocusOut -> Just RgfwEvFocusLost
  R.EventKeyPress k m -> Just (RgfwEvKey k m True)
  R.EventKeyRepeat k m -> Just (RgfwEvKey k m True)
  R.EventKeyRelease k m -> Just (RgfwEvKey k m False)
  R.EventKeyChar ch | isPrint ch -> Just (RgfwEvChar ch)
  _ -> Nothing

-- | Accumulate a decoded event into frame input. The caller handles close
-- events separately; motion coordinates are already scaled by decoding.
applyRgfwEvent :: Input -> RgfwEvent -> Input
applyRgfwEvent inp ev = case ev of
  RgfwEvClose -> inp
  RgfwEvMotion x y -> inp {inputMousePos = V2 x y}
  -- RGFW numbers buttons from 0 in 'mouseButtonNumber' order: left, middle,
  -- right, back, forward, then the rest.
  RgfwEvButton btn down -> applyMouseButton (mouseButtonNumber (fromIntegral btn + 1)) down inp
  RgfwEvLeave -> applyPointerLeave inp
  RgfwEvScroll dx dy -> inp {inputScroll = let V2 sx sy = inputScroll inp in V2 (sx + dx) (sy + dy)}
  RgfwEvChar c -> inp {inputChars = inputChars inp ++ [c]}
  -- 'applyKey' detects auto-repeat because the key is already held.
  RgfwEvKey k m down ->
    (maybe inp (\key -> applyKey key down inp) (mapRgfwKey k m)) {inputModifiers = modsFromRgfw m}
  RgfwEvFocusLost -> releaseAllKeys inp

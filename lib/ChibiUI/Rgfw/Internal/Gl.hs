-- | OpenGL presentation for the RGFW host, adapted from nano-ui-rgfw.
--
-- Geometry goes to the GPU straight from the frame's 'DrawData' buffers,
-- one scissored draw per command; commands whose texture id names an image
-- sample the texture 'uploadImagesGl' maintains, and text commands sample
-- the coverage atlas the font rasterizes into, synced by 'renderFrameGl'.
--
-- Frames draw into a retained framebuffer and a present copies it to the
-- window. Chibi-ui repaints every frame in full, so the retained buffer
-- only saves the swap from reading the frame back.
module ChibiUI.Rgfw.Internal.Gl
  ( GlRenderer
  , newGlRenderer
  , freeGlRenderer
  , renderFrameGl
  , uploadImagesGl
  , readRetainedPixels
  ) where

import Control.Monad (forM_, when, void)
import Data.Bits (shiftR, (.&.))
import Data.IORef
import Data.Int (Int32)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Internal as BSI
import qualified Data.ByteString.Unsafe as BSU
import qualified Data.IntMap.Strict as IM
import Data.Word (Word8, Word32)
import Foreign.ForeignPtr (withForeignPtr)
import Foreign.Ptr (Ptr, castPtr, nullPtr)
import ChibiUI.Internal.Context (ImageEntry (..))
import ChibiUI.Internal.Draw
import ChibiUI.Internal.Font
import ChibiUI.Internal.Types

-- | Opaque C renderer state (cbits/chibi_ui_gl.c).
data ChibiUiGl

foreign import ccall unsafe "chibi_ui_gl_create"
  c_create :: IO (Ptr ChibiUiGl)

foreign import ccall unsafe "chibi_ui_gl_destroy"
  c_destroy :: Ptr ChibiUiGl -> IO ()

foreign import ccall unsafe "chibi_ui_gl_upload_atlas"
  c_uploadAtlas :: Ptr ChibiUiGl -> Ptr Word8 -> Int32 -> Int32 -> IO Int32

foreign import ccall unsafe "chibi_ui_gl_upload_image"
  c_uploadImage :: Ptr ChibiUiGl -> Int32 -> Int32 -> Int32 -> Ptr Word8 -> IO Int32

foreign import ccall unsafe "chibi_ui_gl_begin"
  c_begin :: Ptr ChibiUiGl -> Int32 -> Int32 -> Float -> Float -> Float -> Float -> Int32 -> IO Int32

foreign import ccall unsafe "chibi_ui_gl_present"
  c_present :: Ptr ChibiUiGl -> IO ()

foreign import ccall unsafe "chibi_ui_gl_read_retained"
  c_readRetained :: Ptr ChibiUiGl -> Ptr Word8 -> IO ()

foreign import ccall unsafe "chibi_ui_gl_upload_geometry"
  c_uploadGeometry :: Ptr ChibiUiGl -> Ptr Word8 -> Int32 -> Ptr Word8 -> Int32 -> IO ()

foreign import ccall unsafe "chibi_ui_gl_draw_geometry"
  c_drawGeometry :: Ptr ChibiUiGl -> Int32 -> Int32 -> Int32 -> Int32 -> Word32 -> Word32 -> Int32 -> IO ()

-- | GPU resources and the font/image sync state owned by one OpenGL
-- context. Release with 'freeGlRenderer' while that context is still
-- current.
data GlRenderer = GlRenderer
  { glHandle :: !(Ptr ChibiUiGl)
  , glAtlas :: !(IORef (Int, Int))
  -- ^ The atlas size last uploaded, to detect resizes.
  , glImages :: !(IORef (IM.IntMap Int))
  -- ^ Per image id, the version last uploaded.
  }

-- | Build the renderer on the calling thread's current OpenGL context.
newGlRenderer :: IO GlRenderer
newGlRenderer = do
  h <- c_create
  when (h == nullPtr) $
    fail "chibi-ui: OpenGL renderer setup failed (needs an OpenGL 3.2 core context)"
  GlRenderer h <$> newIORef (0, 0) <*> newIORef IM.empty

-- | Release the GPU objects (the context must still be current).
freeGlRenderer :: GlRenderer -> IO ()
freeGlRenderer r = c_destroy (glHandle r)

-- | Draw a frame in full and copy it to the window's back buffer. The
-- caller swaps. @scale@ is device pixels per logical pixel and must match
-- the scale the font rasterizes at.
renderFrameGl :: GlRenderer -> Font -> Float -> Int -> Int -> Color -> DrawData -> IO ()
renderFrameGl r font !scale !fbW !fbH bg drawData = do
  let !h = glHandle r
      (!bgR, !bgG, !bgB, _) = colorFloats bg
  began <- c_begin h (fromIntegral fbW) (fromIntegral fbH) scale bgR bgG bgB 1
  when (began == 0) $ fail "chibi-ui: retained framebuffer setup failed"
  syncFontAtlasGl r font
  withForeignPtr (drawVertices drawData) $ \vp ->
    withForeignPtr (drawIndices drawData) $ \ip ->
      c_uploadGeometry
        h
        vp
        (fromIntegral (drawVertexCount drawData))
        ip
        (fromIntegral (drawIndexCount drawData))
  forM_ (drawCommands drawData) $ \cmd ->
    when (cmdIndexCount cmd >= 3) $
      case physClip scale fbW fbH (Rect (cmdClipX cmd) (cmdClipY cmd) (cmdClipW cmd) (cmdClipH cmd)) of
        Nothing -> pure ()
        Just (x0, y0, x1, y1) ->
          c_drawGeometry
            h
            (fromIntegral x0)
            (fromIntegral y0)
            (fromIntegral x1)
            (fromIntegral y1)
            (cmdIndexOffset cmd)
            (cmdIndexCount cmd)
            (fromIntegral (cmdTextureId cmd))
  c_present h

-- | The retained frame's pixels, RGBA rows bottom row first. For debugging
-- what a frame drew.
readRetainedPixels :: GlRenderer -> Int -> Int -> IO BS.ByteString
readRetainedPixels r w h = BSI.create (w * h * 4) (c_readRetained (glHandle r))

-- | Upload the font's coverage atlas when glyphs were added since the last
-- sync, or when its size changed.
syncFontAtlasGl :: GlRenderer -> Font -> IO ()
syncFontAtlasGl r font = do
  dirty <- fontTakeDirty font
  lastWH <- readIORef (glAtlas r)
  atlasWH <- fontAtlasSize font
  if not (dirty || lastWH /= atlasWH)
    then pure ()
    else do
      let (w, h) = atlasWH
      pixels <- fontAtlasPixels font
      ok <- (/= 0) <$> c_uploadAtlas (glHandle r) pixels (fromIntegral w) (fromIntegral h)
      when ok (writeIORef (glAtlas r) atlasWH)

-- | Upload registered images whose version changed since the last sync.
-- Call before 'renderFrameGl' with the GL context current.
uploadImagesGl :: GlRenderer -> IM.IntMap ImageEntry -> IO ()
uploadImagesGl r images = do
  uploaded <- readIORef (glImages r)
  let changed =
        [ (img, e)
        | (img, e) <- IM.toList images
        , IM.lookup img uploaded /= Just (ieVersion e)
        ]
  forM_ changed $ \(img, e) ->
    BSU.unsafeUseAsCString (iePixels e) $ \p ->
      void $
        c_uploadImage
          (glHandle r)
          (fromIntegral img)
          (fromIntegral (ieWidth e))
          (fromIntegral (ieHeight e))
          (castPtr p)
  writeIORef (glImages r) (IM.map ieVersion images `IM.union` uploaded)

-- | Scale a logical clip rect to physical pixels and intersect it with a
-- w x h target, as @(x0, y0, x1, y1)@ with exclusive ends; 'Nothing' if
-- empty.
{-# INLINE physClip #-}
physClip :: Float -> Int -> Int -> Rect -> Maybe (Int, Int, Int, Int)
physClip !scale !w !h (Rect x y rw rh) =
  let !x0 = roundHalfUp (x * scale)
      !y0 = roundHalfUp (y * scale)
      !x1 = roundHalfUp ((x + rw) * scale)
      !y1 = roundHalfUp ((y + rh) * scale)
      !cx0 = max 0 x0
      !cy0 = max 0 y0
      !cx1 = min w x1
      !cy1 = min h y1
   in if cx0 >= cx1 || cy0 >= cy1 then Nothing else Just (cx0, cy0, cx1, cy1)

colorFloats :: Color -> (Float, Float, Float, Float)
colorFloats (Color w) = (chan 24, chan 16, chan 8, chan 0)
  where
    chan s = fromIntegral ((w `shiftR` s) .&. 0xFF) / 255

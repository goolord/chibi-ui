-- | OpenGL presentation for the RGFW host, adapted from nano-ui-rgfw.
--
-- Geometry goes to the GPU straight from the frame's 'DrawData' buffers,
-- one draw per texture run; commands whose texture id names an image
-- sample the texture 'uploadImagesGl' maintains, and text commands sample
-- the coverage atlas the font rasterizes into, both synced by
-- 'renderFrameGl'.
--
-- Frames draw into a retained framebuffer and a present copies it to the
-- window. The frame's 'Damage' decides how much of that buffer to touch:
-- a damage-free frame presents without drawing, a few rectangles are
-- cleared and repainted scissored to the damage, and a full frame is the
-- whole clear-and-draw as before.
module ChibiUI.Rgfw.Internal.Gl
  ( GlRenderer
  , newGlRenderer
  , freeGlRenderer
  , renderFrameGl
  , readRetainedPixels
  ) where

import Control.Monad (forM, forM_, unless, when)
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
import ChibiUI.Internal.Damage (Damage (..), commandQuadBounds)
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
  c_uploadGeometry :: Ptr ChibiUiGl -> Ptr Word8 -> Int32 -> IO ()

foreign import ccall unsafe "chibi_ui_gl_draw_geometry"
  c_drawGeometry :: Ptr ChibiUiGl -> Int32 -> Int32 -> Int32 -> Int32 -> Word32 -> Word32 -> Int32 -> IO ()

foreign import ccall unsafe "chibi_ui_gl_clear_region"
  c_clearRegion :: Ptr ChibiUiGl -> Int32 -> Int32 -> Int32 -> Int32 -> Float -> Float -> Float -> IO ()

-- | GPU resources and the font/image sync state owned by one OpenGL
-- context. Release with 'freeGlRenderer' while that context is still
-- current.
data GlRenderer = GlRenderer
  { glHandle :: !(Ptr ChibiUiGl)
  , glAtlasUploaded :: !(IORef Bool)
  -- ^ Whether the atlas texture exists yet.
  , glImages :: !(IORef (IM.IntMap Int))
  -- ^ Per image id, the version last uploaded.
  , glFrameSize :: !(IORef (Int, Int))
  -- ^ The framebuffer size of the last drawn frame. A size change
  -- recreates the retained texture, discarding its pixels, so it forces a
  -- full frame.
  }

-- | Build the renderer on the calling thread's current OpenGL context.
newGlRenderer :: IO GlRenderer
newGlRenderer = do
  h <- c_create
  when (h == nullPtr) $
    fail "chibi-ui: OpenGL renderer setup failed (needs an OpenGL 3.2 core context)"
  GlRenderer h <$> newIORef False <*> newIORef IM.empty <*> newIORef (0, 0)

-- | Release the GPU objects (the context must still be current).
freeGlRenderer :: GlRenderer -> IO ()
freeGlRenderer r = c_destroy (glHandle r)

-- | Draw a frame into the retained framebuffer and copy it to the window's
-- back buffer; the caller swaps. @scale@ is device pixels per logical
-- pixel and must match the scale the font rasterizes at. The @damage@ the
-- caller tracked against the last frame decides the work: nothing, the
-- damaged rectangles, or everything. New texture contents (atlas glyphs or
-- image versions) and a framebuffer size change repaint in full.
renderFrameGl :: GlRenderer -> Font -> IM.IntMap ImageEntry -> Float -> Int -> Int -> Color -> DrawData -> Damage -> IO ()
renderFrameGl r font images !scale !fbW !fbH bg drawData damage = do
  let !h = glHandle r
      (!bgR, !bgG, !bgB, _) = colorFloats bg
  atlasChanged <- syncFontAtlasGl r font
  imagesChanged <- uploadImagesGl r images
  lastSize <- readIORef (glFrameSize r)
  let !full = atlasChanged || imagesChanged || lastSize /= (fbW, fbH) || damage == DamageFull
  if damage == DamageNone && not full
    then c_present h
    else do
      began <- c_begin h (fromIntegral fbW) (fromIntegral fbH) scale bgR bgG bgB (if full then 1 else 0)
      when (began == 0) $ fail "chibi-ui: retained framebuffer setup failed"
      uploadGeometry h drawData
      case damage of
        DamageRects rs | not full -> drawDamaged h scale fbW fbH drawData rs bgR bgG bgB
        _ -> mapM_ (drawCmd h (0, 0, fbW, fbH)) (drawCommands drawData)
      c_present h
  writeIORef (glFrameSize r) (fbW, fbH)

-- | Hand the frame's vertices to the GPU. Partial and full frames alike
-- upload everything; the draws below pick what to rasterize.
uploadGeometry :: Ptr ChibiUiGl -> DrawData -> IO ()
uploadGeometry h drawData =
  withForeignPtr (drawVertices drawData) $ \vp ->
    c_uploadGeometry h vp (fromIntegral (drawVertexCount drawData))

-- | Draw one command scissored to a physical-pixel box. Quads arrive
-- already cut to their clips, so the scissor only bounds damage repaints.
drawCmd :: Ptr ChibiUiGl -> (Int, Int, Int, Int) -> DrawCmd -> IO ()
drawCmd h (x0, y0, x1, y1) cmd =
  when (cmdQuadCount cmd > 0) $
    c_drawGeometry
      h
      (fromIntegral x0)
      (fromIntegral y0)
      (fromIntegral x1)
      (fromIntegral y1)
      (cmdFirstQuad cmd)
      (cmdQuadCount cmd)
      (fromIntegral (cmdTextureId cmd))

-- | Clear the damaged rectangles, then redraw each command whose quads
-- meet one, scissored to it, into the still-retained pixels around it.
drawDamaged :: Ptr ChibiUiGl -> Float -> Int -> Int -> DrawData -> [Rect] -> Float -> Float -> Float -> IO ()
drawDamaged h !scale !fbW !fbH drawData rects bgR bgG bgB = do
  let boxes = [(dmg, box) | dmg <- rects, Just box <- [physClip scale fbW fbH dmg]]
  forM_ boxes $ \(_, (x0, y0, x1, y1)) ->
    c_clearRegion h (fromIntegral x0) (fromIntegral y0) (fromIntegral x1) (fromIntegral y1) bgR bgG bgB
  forM_ (drawCommands drawData) $ \cmd -> do
    bounds <- commandQuadBounds drawData cmd
    forM_ boxes $ \(dmg, box) -> when (rectsOverlap bounds dmg) (drawCmd h box cmd)

-- | The retained frame's pixels, RGBA rows bottom row first. For debugging
-- what a frame drew.
readRetainedPixels :: GlRenderer -> Int -> Int -> IO BS.ByteString
readRetainedPixels r w h = BSI.create (w * h * 4) (c_readRetained (glHandle r))

-- | Upload the font's coverage atlas the first time, and again whenever
-- glyphs were added since the last sync. Reports whether the texture
-- changed, so callers can repaint in full: baked glyph positions may have
-- moved.
syncFontAtlasGl :: GlRenderer -> Font -> IO Bool
syncFontAtlasGl r font = do
  dirty <- fontTakeDirty font
  uploaded <- readIORef (glAtlasUploaded r)
  if uploaded && not dirty
    then pure False
    else do
      let (w, h) = atlasSize
      pixels <- fontAtlasPixels font
      ok <- (/= 0) <$> c_uploadAtlas (glHandle r) pixels (fromIntegral w) (fromIntegral h)
      when ok (writeIORef (glAtlasUploaded r) True)
      pure ok

-- | Upload registered images whose version changed since the last sync.
-- Reports whether any texture changed, so callers can repaint in full: the
-- same quads sample different pixels.
uploadImagesGl :: GlRenderer -> IM.IntMap ImageEntry -> IO Bool
uploadImagesGl r images = do
  uploaded <- readIORef (glImages r)
  let changed =
        [ (img, e)
        | (img, e) <- IM.toList images
        , IM.lookup img uploaded /= Just (ieVersion e)
        ]
  oks <- forM changed $ \(img, e) ->
    BSU.unsafeUseAsCString (iePixels e) $ \p -> do
      ok <-
        c_uploadImage
          (glHandle r)
          (fromIntegral img)
          (fromIntegral (ieWidth e))
          (fromIntegral (ieHeight e))
          (castPtr p)
      pure (ok /= 0)
  -- Unchanged frames, the common case, leave the map as it is.
  unless (null changed) $
    writeIORef (glImages r) (foldl' (\m (img, e) -> IM.insert img (ieVersion e) m) uploaded changed)
  pure (or oks)

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

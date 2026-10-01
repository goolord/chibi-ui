-- | OpenGL presentation for the RGFW host, adapted from nano-ui-rgfw.
--
-- Geometry goes to the GPU straight from the frame's 'DrawData' buffers,
-- one draw per texture run; commands whose texture id names an image
-- sample the texture 'uploadImagesGl' maintains, and text commands sample
-- the coverage atlas the font rasterizes into, both synced by
-- 'renderFrameGl'.
--
-- Frames draw into a retained framebuffer and a present copies it to the
-- window. The renderer keeps a snapshot of the frame its vertex buffer
-- holds and diffs each new frame against it: the diff's 'Upload' says how
-- much geometry to send, and its 'Damage' how much of the framebuffer to
-- touch:
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
import ChibiUI.Internal.Damage (Damage (..), FrameSnapshot, Upload (..), trackUploads)
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

foreign import ccall unsafe "chibi_ui_gl_upload_atlas_rows"
  c_uploadAtlasRows :: Ptr ChibiUiGl -> Ptr Word8 -> Int32 -> Int32 -> Int32 -> IO ()

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

foreign import ccall unsafe "chibi_ui_gl_upload_quads"
  c_uploadQuads :: Ptr ChibiUiGl -> Ptr Word8 -> Word32 -> Word32 -> IO ()

foreign import ccall unsafe "chibi_ui_gl_draw_geometry"
  c_drawGeometry :: Ptr ChibiUiGl -> Int32 -> Int32 -> Int32 -> Int32 -> Word32 -> Word32 -> Int32 -> IO ()

foreign import ccall unsafe "chibi_ui_gl_clear_region"
  c_clearRegion :: Ptr ChibiUiGl -> Int32 -> Int32 -> Int32 -> Int32 -> Float -> Float -> Float -> IO ()

-- | GPU resources and the font/image sync state owned by one OpenGL
-- context. Release with 'freeGlRenderer' while that context is still
-- current.
data GlRenderer = GlRenderer
  { glHandle :: !(Ptr ChibiUiGl)
  , glImages :: !(IORef (IM.IntMap Int))
  -- ^ Per image id, the version last uploaded.
  , glSnapshot :: !(IORef (Maybe FrameSnapshot))
  -- ^ The frame the vertex buffer holds, to diff the next one against;
  -- 'Nothing' before the first upload.
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
  GlRenderer h <$> newIORef IM.empty <*> newIORef Nothing <*> newIORef (0, 0)

-- | Release the GPU objects (the context must still be current).
freeGlRenderer :: GlRenderer -> IO ()
freeGlRenderer r = c_destroy (glHandle r)

-- | Draw a frame into the retained framebuffer and copy it to the window's
-- back buffer; the caller swaps. @scale@ is device pixels per logical
-- pixel and must match the scale the font rasterizes at, and @window@ is
-- the frame's logical size. The frame's damage against the last one drawn
-- decides the work: nothing, the damaged rectangles, or everything. A new
-- atlas, new image versions and a framebuffer size change repaint in full;
-- glyphs added to the atlas do not, as they only fill texels no quad
-- sampled before. Only the quads that changed are uploaded.
renderFrameGl :: GlRenderer -> Font -> IM.IntMap ImageEntry -> Float -> Int -> Int -> Color -> Size -> DrawData -> IO ()
renderFrameGl r font images !scale !fbW !fbH bg window drawData = do
  let !h = glHandle r
      (!bgR, !bgG, !bgB, _) = colorFloats bg
  (damage, upload) <- trackUploads (glSnapshot r) window drawData
  atlasChanged <- syncFontAtlasGl r font
  imagesChanged <- uploadImagesGl r images
  lastSize <- readIORef (glFrameSize r)
  let !full = atlasChanged || imagesChanged || lastSize /= (fbW, fbH) || damage == DamageFull
  if damage == DamageNone && not full
    then c_present h
    else do
      began <- c_begin h (fromIntegral fbW) (fromIntegral fbH) scale bgR bgG bgB (if full then 1 else 0)
      when (began == 0) $ fail "chibi-ui: retained framebuffer setup failed"
      uploadGeometry h drawData upload
      case damage of
        DamageRects rs | not full -> drawDamaged h scale fbW fbH drawData rs bgR bgG bgB
        _ -> mapM_ (drawCmd h (0, 0, fbW, fbH)) (drawCommands drawData)
      c_present h
  writeIORef (glFrameSize r) (fbW, fbH)

-- | Hand the frame's vertices to the GPU, as the diff against the frame
-- the buffer holds asks: nothing, the changed quads in runs, or all.
uploadGeometry :: Ptr ChibiUiGl -> DrawData -> Upload -> IO ()
uploadGeometry h drawData upload =
  withForeignPtr (drawVertices drawData) $ \vp -> case upload of
    UploadNone -> pure ()
    UploadQuads qs ->
      forM_ (quadRuns qs) $ \(q, k) -> c_uploadQuads h vp (fromIntegral q) (fromIntegral k)
    UploadAll -> c_uploadGeometry h vp (fromIntegral (drawVertexCount drawData))

-- | Ascending quad indices as runs of @(first, count)@.
quadRuns :: [Int] -> [(Int, Int)]
quadRuns = foldr step []
  where
    step q ((q', k) : rest) | q' == q + 1 = (q, k + 1) : rest
    step q runs = (q, 1) : runs

-- | Draw one command scissored to a physical-pixel box. Quads arrive
-- already cut to their clips, so the scissor only bounds damage repaints.
drawCmd :: Ptr ChibiUiGl -> (Int, Int, Int, Int) -> DrawCmd -> IO ()
drawCmd h (x0, y0, x1, y1) cmd =
  when (cmdQuadCount cmd > 0) $
    c_drawGeometry h (fromIntegral x0) (fromIntegral y0) (fromIntegral x1) (fromIntegral y1)
      (cmdFirstQuad cmd) (cmdQuadCount cmd) (fromIntegral (cmdTextureId cmd))

-- | Clear the damaged rectangles, then redraw every command scissored to
-- each, into the still-retained pixels around it. Flat shapes and text
-- share one batch, so a command nearly always spans the damage anyway;
-- the scissor discards the rest on the GPU.
drawDamaged :: Ptr ChibiUiGl -> Float -> Int -> Int -> DrawData -> [Rect] -> Float -> Float -> Float -> IO ()
drawDamaged h !scale !fbW !fbH drawData rects bgR bgG bgB = do
  let boxes = [box | dmg <- rects, Just box <- [physClip scale fbW fbH dmg]]
  forM_ boxes $ \(x0, y0, x1, y1) ->
    c_clearRegion h (fromIntegral x0) (fromIntegral y0) (fromIntegral x1) (fromIntegral y1) bgR bgG bgB
  forM_ (drawCommands drawData) $ \cmd -> forM_ boxes $ \box -> drawCmd h box cmd

-- | The retained frame's pixels, RGBA rows bottom row first. For debugging
-- what a frame drew.
readRetainedPixels :: GlRenderer -> Int -> Int -> IO BS.ByteString
readRetainedPixels r w h = BSI.create (w * h * 4) (c_readRetained (glHandle r))

-- | Upload what changed in the font's coverage atlas: all of a new one,
-- or the rows new glyphs landed in. Reports whether the atlas is new, so
-- callers repaint in full: every glyph moved.
syncFontAtlasGl :: GlRenderer -> Font -> IO Bool
syncFontAtlasGl r font = do
  change <- fontTakeDirty font
  let (w, h) = atlasSize
  case change of
    AtlasClean -> pure False
    AtlasRows y0 y1 -> do
      pixels <- fontAtlasPixels font
      when (pixels /= nullPtr) $
        c_uploadAtlasRows (glHandle r) pixels (fromIntegral w) (fromIntegral y0) (fromIntegral y1)
      pure False
    AtlasFresh -> do
      pixels <- fontAtlasPixels font
      (/= 0) <$> c_uploadAtlas (glHandle r) pixels (fromIntegral w) (fromIntegral h)

-- | Upload registered images whose version changed since the last sync.
-- Reports whether any texture changed, so callers can repaint in full: the
-- same quads sample different pixels.
uploadImagesGl :: GlRenderer -> IM.IntMap ImageEntry -> IO Bool
uploadImagesGl r images = do
  uploaded <- readIORef (glImages r)
  let changed = IM.differenceWith (\e v -> if v == ieVersion e then Nothing else Just e) images uploaded
  oks <- forM (IM.toList changed) $ \(img, e) ->
    BSU.unsafeUseAsCString (iePixels e) $ \p ->
      (/= 0) <$> c_uploadImage (glHandle r) (fromIntegral img) (fromIntegral (ieWidth e)) (fromIntegral (ieHeight e)) (castPtr p)
  -- Unchanged frames, the common case, leave the map as it is.
  unless (IM.null changed) $ writeIORef (glImages r) (IM.union (ieVersion <$> changed) uploaded)
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

{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}

-- | The draw list: widgets emit quads into a growable vertex buffer,
-- batched into commands that share a texture. Every quad is cut to the
-- current clip as it is emitted, so commands carry no clip and the
-- renderer needs no scissor to honour one. The vertex layout is nano-ui
-- RGFW's 32-byte vertex: logical position, RGBA as four floats, and UV.
-- A quad is four vertices in order (top-left, top-right, bottom-right,
-- bottom-left); its two triangles are always the same six indices over
-- them, so the renderer keeps a fixed index buffer and the draw list holds
-- none.
--
-- Texture ids: 0 is the atlas, which glyphs sample and flat geometry
-- shares with a UV of -1 (full coverage), so text and the shapes around it
-- stay in one batch; 1 or more are backend-registered images.
module ChibiUI.Internal.Draw
  ( DrawArena
  , DrawCmd (..)
  , DrawData (..)
  , newDrawArena
  , resetDrawArena
  , finishFrame
  , texAtlas
  , texImage
  , pushClip
  , popClip
  , currentClip
  , fillRect
  , strokeRect
  , emitQuadUV
  , vertexSize
  , quadBytes
  ) where

import Control.Monad (when)
import Data.IORef
import Data.Maybe (fromMaybe)
import Data.Word (Word8, Word32)
import Foreign.ForeignPtr (ForeignPtr, mallocForeignPtrBytes, withForeignPtr)
import Foreign.Marshal.Utils (copyBytes)
import Foreign.Ptr (Ptr, castPtr, plusPtr)
import Foreign.Storable (pokeByteOff)
import ChibiUI.Internal.Types
  ( Color
  , Rect (..)
  , colorFloats
  , rectIntersect
  )
import ChibiUI.Internal.URef

-- | Packed vertex stride in bytes: 32.
vertexSize :: Int
vertexSize = 32

-- | Bytes per quad: four vertices.
quadBytes :: Int
quadBytes = 4 * vertexSize

-- | The texture id of the atlas: glyphs, and flat geometry.
texAtlas :: Int
texAtlas = 0

-- | The draw-list texture id of a registered image. Image ids are 0-based.
{-# INLINE texImage #-}
texImage :: Int -> Int
texImage img = img + 1

-- | One draw batch: a run of quads sampling one texture.
data DrawCmd = DrawCmd
  { cmdTextureId :: {-# UNPACK #-} !Int
  , cmdFirstQuad :: {-# UNPACK #-} !Word32
  , cmdQuadCount :: {-# UNPACK #-} !Word32
  }
  deriving (Eq, Show)

-- | One frame's geometry and batches. The vertex pointer refers to
-- reusable arena storage: render or copy it before running another frame
-- on the same arena. The count describes the used prefix, not the
-- capacity.
data DrawData = DrawData
  { drawVertices :: ForeignPtr Word8
  , drawVertexCount :: !Int
  , drawCommands :: [DrawCmd]
  }

-- | A growable vertex buffer for one window's frames. Emit with
-- 'fillRect' and friends; snapshot with 'finishFrame'.
data DrawArena = DrawArena
  { daVertex :: !(IORef (ForeignPtr Word8))
  , daVertexPtr :: !(IORef (Ptr Word8))
  -- ^ The vertex buffer's base address, cached so quad emission writes
  -- through it without a keep-alive per quad. Valid between growths: the
  -- buffer is pinned memory and the 'ForeignPtr' stays referenced by
  -- 'daVertex'.
  , daVertexCap :: !(IORef Int)
  , daVertexCount :: !URef
  , daCommands :: !(IORef [DrawCmd])
  -- ^ Closed batches, most recent first. The batch still being extended
  -- lives in the @daBatch*@ refs below instead, so a quad that continues
  -- its batch allocates nothing.
  , daLastClip :: !(IORef Rect)
  , daClipStack :: !(IORef [Rect])
  , daBatchTexture :: !(IORef Int)
  , daBatchStart :: !URef
  , daBatchCount :: !URef
  -- ^ The open batch: its texture, its first quad, and its quad count so
  -- far. A count of zero means no batch is open.
  }

-- | A brand-new arena, empty as 'resetDrawArena' leaves one.
newDrawArena :: IO DrawArena
newDrawArena = do
  vbuf <- newBuffer 0
  daVertexPtr <- withForeignPtr vbuf (newIORef . castPtr)
  daVertex <- newIORef vbuf
  daVertexCap <- newIORef 0
  daVertexCount <- newURef 0
  daCommands <- newIORef []
  daLastClip <- newIORef infiniteClip
  daClipStack <- newIORef []
  daBatchTexture <- newIORef texAtlas
  daBatchStart <- newURef 0
  daBatchCount <- newURef 0
  pure DrawArena {..}

newBuffer :: Int -> IO (ForeignPtr Word8)
newBuffer = mallocForeignPtrBytes . max 1

-- | The clip a fresh frame starts with: everything.
infiniteClip :: Rect
infiniteClip = Rect 0 0 1e9 1e9

-- | Drop the frame's geometry and reset the clip. The buffer keeps its
-- capacity.
resetDrawArena :: DrawArena -> IO ()
resetDrawArena a = do
  writeURef (daVertexCount a) 0
  writeIORef (daCommands a) []
  writeIORef (daLastClip a) infiniteClip
  writeIORef (daClipStack a) []
  writeIORef (daBatchTexture a) texAtlas
  writeURef (daBatchStart a) 0
  writeURef (daBatchCount a) 0

-- | Snapshot the frame. The arena must not be reset and emitted into again
-- until the backend has rendered or copied the snapshot. Reading the open
-- batch without closing it keeps the snapshot repeatable.
finishFrame :: DrawArena -> IO DrawData
finishFrame a = do
  vfp <- readIORef (daVertex a)
  vc <- readURef (daVertexCount a)
  pend <- pendingCmd a
  closed <- readIORef (daCommands a)
  pure
    DrawData
      { drawVertices = vfp
      , drawVertexCount = vc
      , drawCommands = reverse (pend ++ closed)
      }

-- | The open batch as a command, if one is open.
pendingCmd :: DrawArena -> IO [DrawCmd]
pendingCmd a = do
  n <- readURef (daBatchCount a)
  if n <= 0
    then pure []
    else do
      t <- readIORef (daBatchTexture a)
      s <- readURef (daBatchStart a)
      pure [DrawCmd t (fromIntegral s) (fromIntegral n)]

-- | Grow the vertex buffer to at least @need@ bytes, keeping the old
-- contents and refreshing the cached base pointer. Inlined, so the
-- steady-state capacity check allocates nothing.
{-# INLINE growVertices #-}
growVertices :: DrawArena -> Int -> IO ()
growVertices a !need = do
  cap <- readIORef (daVertexCap a)
  when (need > cap) $ do
    let cap' = max need (max 4096 (cap * 2))
    fp <- readIORef (daVertex a)
    fp' <- newBuffer cap'
    withForeignPtr fp $ \src ->
      withForeignPtr fp' $ \dst -> copyBytes dst src cap
    writeIORef (daVertex a) fp'
    writeIORef (daVertexCap a) cap'
    withForeignPtr fp' $ \p -> writeIORef (daVertexPtr a) p

-- | The current clip, in window coordinates. Emitters cut every quad to it.
currentClip :: DrawArena -> IO Rect
currentClip a = readIORef (daLastClip a)

-- | Push @r@ intersected with the current clip. Every command emitted until
-- the matching 'popClip' is cut to the result.
pushClip :: DrawArena -> Rect -> IO ()
pushClip a r = do
  cur <- readIORef (daLastClip a)
  modifyIORef' (daClipStack a) (cur :)
  writeIORef (daLastClip a) (clipIntersect cur r)

-- | The shared positive area of two rectangles, or the empty rectangle at
-- the origin when they are disjoint or touch edges. Inline, so a push
-- allocates only the saved clip and the intersection itself.
{-# INLINE clipIntersect #-}
clipIntersect :: Rect -> Rect -> Rect
clipIntersect a b = fromMaybe (Rect 0 0 0 0) (rectIntersect a b)

-- | Restore the clip pushed last.
popClip :: DrawArena -> IO ()
popClip a = do
  stack <- readIORef (daClipStack a)
  case stack of
    (r : rest) -> do
      writeIORef (daClipStack a) rest
      writeIORef (daLastClip a) r
    [] -> pure ()

-- | Append a quad sampling texture @tex@ under the current clip. The quad
-- is @x0, y0, x1, y1@ in logical pixels with the given colour, and UV
-- corners. A texture change starts a new batch.
{-# INLINE emitQuadUV #-}
emitQuadUV :: DrawArena -> Int -> Float -> Float -> Float -> Float -> Color -> Float -> Float -> Float -> Float -> IO ()
emitQuadUV a !tex !x0 !y0 !x1 !y1 col !u0 !v0 !u1 !v1 = do
  Rect cx0 cy0 cw ch <- readIORef (daLastClip a)
  let !cx1 = cx0 + cw
      !cy1 = cy0 + ch
  if x1 <= x0 || y1 <= y0 || cx1 <= cx0 || cy1 <= cy0
      || x0 >= cx1 || x1 <= cx0 || y0 >= cy1 || y1 <= cy0
    then pure ()
    else
      if x0 >= cx0 && y0 >= cy0 && x1 <= cx1 && y1 <= cy1
        -- Inside the clip, the usual case: nothing to cut.
        then appendQuad a tex x0 y0 x1 y1 col u0 v0 u1 v1
        else do
          -- Cut the quad to the clip, sliding the UVs with the edges.
          let !nx0 = max x0 cx0
              !ny0 = max y0 cy0
              !nx1 = min x1 cx1
              !ny1 = min y1 cy1
              ux x = u0 + (u1 - u0) * ((x - x0) / (x1 - x0))
              uy y = v0 + (v1 - v0) * ((y - y0) / (y1 - y0))
          appendQuad a tex nx0 ny0 nx1 ny1 col (ux nx0) (uy ny0) (ux nx1) (uy ny1)

-- | Write a quad after the arena's last, growing the buffer when needed,
-- and batch it.
{-# INLINE appendQuad #-}
appendQuad :: DrawArena -> Int -> Float -> Float -> Float -> Float -> Color -> Float -> Float -> Float -> Float -> IO ()
appendQuad a !tex !x0 !y0 !x1 !y1 col !u0 !v0 !u1 !v1 = do
  n <- readURef (daVertexCount a)
  growVertices a ((n + 4) * vertexSize)
  buf <- readIORef (daVertexPtr a)
  writeQuadVertices buf n x0 y0 x1 y1 col u0 v0 u1 v1
  writeURef (daVertexCount a) (n + 4)
  batchQuads a tex (n `quot` 4) 1

-- | A solid rectangle: an atlas quad whose UV of -1 reads as full coverage.
{-# INLINE fillRect #-}
fillRect :: DrawArena -> Rect -> Color -> IO ()
fillRect a (Rect x y w h) c = emitQuadUV a texAtlas x y (x + w) (y + h) c (-1) (-1) (-1) (-1)

-- | A border of @bw@ logical pixels, drawn inside the rectangle's edges.
strokeRect :: DrawArena -> Rect -> Float -> Color -> IO ()
strokeRect a (Rect x y w h) bw c
  | w <= 0 || h <= 0 = pure ()
  | otherwise = do
      let t = min bw (w / 2)
          b = min bw (h / 2)
      fillRect a (Rect x y w t) c
      fillRect a (Rect x (y + h - b) w b) c
      fillRect a (Rect x (y + t) t (h - t - b)) c
      fillRect a (Rect (x + w - t) (y + t) t (h - t - b)) c

-- | Extend the open batch with the @k@ quads from @q@ when their texture
-- continues it; else close the open batch into the command list and open
-- a fresh one. The quads always follow the open batch's, so continuing it
-- only bumps a counter, and the common run under one texture allocates
-- nothing.
{-# INLINE batchQuads #-}
batchQuads :: DrawArena -> Int -> Int -> Int -> IO ()
batchQuads a !texture !q !k = do
  openTex <- readIORef (daBatchTexture a)
  openCount <- readURef (daBatchCount a)
  if openCount > 0 && openTex == texture
    then writeURef (daBatchCount a) (openCount + k)
    else do
      when (openCount > 0) $ do
        openStart <- readURef (daBatchStart a)
        modifyIORef' (daCommands a) (DrawCmd openTex (fromIntegral openStart) (fromIntegral openCount) :)
      writeIORef (daBatchTexture a) texture
      writeURef (daBatchStart a) q
      writeURef (daBatchCount a) k

-- | Write four vertices in the C renderer's 32-byte layout: position, RGBA
-- as four floats, UV. Colour channels come from the packed @0xRRGGBBAA@ word.
writeQuadVertices :: Ptr Word8 -> Int -> Float -> Float -> Float -> Float -> Color -> Float -> Float -> Float -> Float -> IO ()
writeQuadVertices !buf !n !x0 !y0 !x1 !y1 col !u0 !v0 !u1 !v1 = do
  let !base = buf `plusPtr` (n * vertexSize)
      !(r, g, b, a) = colorFloats col
      put !i !x !y !u !v = do
        let !p = base `plusPtr` (i * vertexSize)
        pokeByteOff p 0 x
        pokeByteOff p 4 y
        pokeByteOff p 8 (r :: Float)
        pokeByteOff p 12 (g :: Float)
        pokeByteOff p 16 (b :: Float)
        pokeByteOff p 20 (a :: Float)
        pokeByteOff p 24 (u :: Float)
        pokeByteOff p 28 (v :: Float)
  put 0 x0 y0 u0 v0
  put 1 x1 y0 u1 v0
  put 2 x1 y1 u1 v1
  put 3 x0 y1 u0 v1

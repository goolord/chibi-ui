-- | The draw list: widgets emit quads into growable vertex and index
-- buffers, batched into commands that share a clip rectangle and a texture.
-- The vertex layout is nano-ui RGFW's 32-byte quad: logical position, RGBA
-- as four floats, and UV, so the C renderer needs no adaptation beyond its
-- name. Texture ids: 0 is flat geometry, 1 the glyph atlas, and 2 or more
-- are backend-registered images.
module ChibiUI.Internal.Draw
  ( DrawArena
  , DrawCmd (..)
  , cmdClipRect
  , DrawData (..)
  , newDrawArena
  , resetDrawArena
  , finishFrame
  , texFlat
  , texGlyphAtlas
  , texImage
  , pushClip
  , popClip
  , currentClip
  , setTexture
  , fillRect
  , strokeRect
  , emitQuadUV
  , vertexSize
  , indexSize
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
  ( Color (..)
  , Rect (..)
  , colorA
  , colorB
  , colorG
  , colorR
  , rectIntersect
  )
import ChibiUI.Internal.URef

-- | Packed vertex stride in bytes: 32.
vertexSize :: Int
vertexSize = 32

-- | Index stride in bytes: 4, for a 32-bit unsigned index.
indexSize :: Int
indexSize = 4

-- | Reserved texture id for flat, untextured geometry.
texFlat :: Int
texFlat = 0

-- | Reserved texture id for the baked glyph atlas.
texGlyphAtlas :: Int
texGlyphAtlas = 1

-- | The draw-list texture id of a registered image. Image ids are 0-based.
{-# INLINE texImage #-}
texImage :: Int -> Int
texImage img = img + 2

-- | One draw batch: everything drawn under one clip with one texture.
data DrawCmd = DrawCmd
  { cmdClipX :: {-# UNPACK #-} !Float
  , cmdClipY :: {-# UNPACK #-} !Float
  , cmdClipW :: {-# UNPACK #-} !Float
  , cmdClipH :: {-# UNPACK #-} !Float
  , cmdTextureId :: {-# UNPACK #-} !Int
  , cmdIndexOffset :: {-# UNPACK #-} !Word32
  , cmdIndexCount :: {-# UNPACK #-} !Word32
  }
  deriving (Eq, Show)

-- | A command's clip rectangle in logical pixels.
{-# INLINE cmdClipRect #-}
cmdClipRect :: DrawCmd -> Rect
cmdClipRect c = Rect (cmdClipX c) (cmdClipY c) (cmdClipW c) (cmdClipH c)

-- | One frame's geometry and batches. Vertex and index pointers refer to
-- reusable arena storage: render or copy them before running another frame
-- on the same arena. Counts describe the used prefix, not the capacity.
data DrawData = DrawData
  { drawVertices :: ForeignPtr Word8
  , drawVertexCount :: !Int
  , drawIndices :: ForeignPtr Word8
  , drawIndexCount :: !Int
  , drawCommands :: [DrawCmd]
  }

-- | Growable buffers for one window's frames. Emit with 'fillRect' and
-- friends; snapshot with 'finishFrame'.
data DrawArena = DrawArena
  { daVertex :: !(IORef (ForeignPtr Word8))
  , daVertexPtr :: !(IORef (Ptr Word8))
  -- ^ The vertex buffer's base address, cached so quad emission writes
  -- through it without a keep-alive per quad. Valid between growths: the
  -- buffer is pinned memory and the 'ForeignPtr' stays referenced by
  -- 'daVertex'.
  , daVertexCap :: !(IORef Int)
  , daVertexCount :: !URef
  , daIndex :: !(IORef (ForeignPtr Word8))
  , daIndexPtr :: !(IORef (Ptr Word32))
  -- ^ The index buffer's base address, cached as above.
  , daIndexCap :: !(IORef Int)
  , daIndexCount :: !URef
  , daCommands :: !(IORef [DrawCmd])
  -- ^ Closed batches, most recent first. The batch still being extended
  -- lives in the @daBatch*@ refs below instead, so a quad that continues
  -- its batch allocates nothing.
  , daLastClip :: !(IORef Rect)
  , daLastTexture :: !(IORef Int)
  , daClipStack :: !(IORef [Rect])
  , daBatchClip :: !(IORef Rect)
  , daBatchTexture :: !(IORef Int)
  , daBatchStart :: !URef
  , daBatchCount :: !URef
  -- ^ The open batch: the clip and texture it runs under, its first index,
  -- and its index count so far. A count of zero means no batch is open.
  }

-- | A brand-new arena.
newDrawArena :: IO DrawArena
newDrawArena = do
  vbuf <- newBuffer 0
  ibuf <- newBuffer 0
  vptr <- withForeignPtr vbuf (newIORef . castPtr)
  iptr <- withForeignPtr ibuf (newIORef . (castPtr :: Ptr Word8 -> Ptr Word32))
  vref <- newIORef vbuf
  iref <- newIORef ibuf
  cref <- newIORef []
  lcref <- newIORef infiniteClip
  ltref <- newIORef texFlat
  cstack <- newIORef []
  vcap <- newIORef 0
  icap <- newIORef 0
  vcnt <- newURef 0
  icnt <- newURef 0
  bclip <- newIORef infiniteClip
  btex <- newIORef texFlat
  bstart <- newURef 0
  bcount <- newURef 0
  pure
    DrawArena
      { daVertex = vref
      , daVertexPtr = vptr
      , daVertexCap = vcap
      , daVertexCount = vcnt
      , daIndex = iref
      , daIndexPtr = iptr
      , daIndexCap = icap
      , daIndexCount = icnt
      , daCommands = cref
      , daLastClip = lcref
      , daLastTexture = ltref
      , daClipStack = cstack
      , daBatchClip = bclip
      , daBatchTexture = btex
      , daBatchStart = bstart
      , daBatchCount = bcount
      }

newBuffer :: Int -> IO (ForeignPtr Word8)
newBuffer = mallocForeignPtrBytes . max 1

-- | The clip a fresh frame starts with: everything.
infiniteClip :: Rect
infiniteClip = Rect 0 0 1e9 1e9

-- | Drop the frame's geometry and reset the clip and texture. Buffers keep
-- their capacity.
resetDrawArena :: DrawArena -> IO ()
resetDrawArena a = do
  writeURef (daVertexCount a) 0
  writeURef (daIndexCount a) 0
  writeIORef (daCommands a) []
  writeIORef (daLastClip a) infiniteClip
  writeIORef (daLastTexture a) texFlat
  writeIORef (daClipStack a) []
  writeIORef (daBatchClip a) infiniteClip
  writeIORef (daBatchTexture a) texFlat
  writeURef (daBatchStart a) 0
  writeURef (daBatchCount a) 0

-- | Snapshot the frame. The arena must not be reset and emitted into again
-- until the backend has rendered or copied the snapshot. Reading the open
-- batch without closing it keeps the snapshot repeatable.
finishFrame :: DrawArena -> IO DrawData
finishFrame a = do
  vfp <- readIORef (daVertex a)
  vc <- readURef (daVertexCount a)
  ifp <- readIORef (daIndex a)
  ic <- readURef (daIndexCount a)
  cmds <-
    do pend <- pendingCmd a
       closed <- readIORef (daCommands a)
       pure (reverse (pend ++ closed))
  pure
    DrawData
      { drawVertices = vfp
      , drawVertexCount = vc
      , drawIndices = ifp
      , drawIndexCount = ic
      , drawCommands = cmds
      }

-- | The open batch as a command, if one is open.
pendingCmd :: DrawArena -> IO [DrawCmd]
pendingCmd a = do
  n <- readURef (daBatchCount a)
  if n <= 0
    then pure []
    else do
      Rect x y w h <- readIORef (daBatchClip a)
      t <- readIORef (daBatchTexture a)
      s <- readURef (daBatchStart a)
      pure [DrawCmd x y w h t (fromIntegral s) (fromIntegral n)]

-- | Grow a buffer to at least @need@ bytes, keeping the old contents and
-- refreshing the cached base pointer. Inlined, so the steady-state
-- capacity check allocates nothing.
{-# INLINE growBuffer #-}
growBuffer :: IORef (ForeignPtr Word8) -> IORef (Ptr word) -> IORef Int -> Int -> IO ()
growBuffer fpRef ptrRef capRef !need = do
  cap <- readIORef capRef
  if need <= cap
    then pure ()
    else do
      let cap' = max need (max 4096 (cap * 2))
      fp <- readIORef fpRef
      fp' <- newBuffer cap'
      withForeignPtr fp $ \src ->
        withForeignPtr fp' $ \dst -> copyBytes dst src cap
      writeIORef fpRef fp'
      writeIORef capRef cap'
      withForeignPtr fp' $ \p -> writeIORef ptrRef (castPtr p)

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

-- | Set the texture the next commands sample. A change starts a new batch.
setTexture :: DrawArena -> Int -> IO ()
setTexture a t = writeIORef (daLastTexture a) t

-- | Append a quad's vertices and indices under the current clip and
-- texture. The quad is @x0, y0, x1, y1@ in logical pixels with the given
-- colour, and UV corners for textured batches (ignored by flat geometry).
{-# INLINE emitQuadUV #-}
emitQuadUV :: DrawArena -> Float -> Float -> Float -> Float -> Color -> Float -> Float -> Float -> Float -> IO ()
emitQuadUV a !x0 !y0 !x1 !y1 col !u0 !v0 !u1 !v1 = do
  clip <- readIORef (daLastClip a)
  let !(cx0, cy0, cx1, cy1) = clipEdges clip
  if x1 <= x0 || y1 <= y0 || cx1 <= cx0 || cy1 <= cy0
      || x0 >= cx1 || x1 <= cx0 || y0 >= cy1 || y1 <= cy0
    then pure ()
    else do
      -- Cut the quad to the clip, sliding the UVs with the edges.
      let !nx0 = max x0 cx0
          !ny0 = max y0 cy0
          !nx1 = min x1 cx1
          !ny1 = min y1 cy1
          !w = x1 - x0
          !h = y1 - y0
          ux x = if w > 0 then u0 + (u1 - u0) * ((x - x0) / w) else u0
          uy y = if h > 0 then v0 + (v1 - v0) * ((y - y0) / h) else v0
      base <- pushVertices a nx0 ny0 nx1 ny1 col (ux nx0) (uy ny0) (ux nx1) (uy ny1)
      pushIndices a base
      batchCommand a clip

-- | Corners of a rect as @x0, y0, x1, y1@.
{-# INLINE clipEdges #-}
clipEdges :: Rect -> (Float, Float, Float, Float)
clipEdges (Rect x y w h) = (x, y, x + w, y + h)

-- | A solid rectangle.
{-# INLINE fillRect #-}
fillRect :: DrawArena -> Rect -> Color -> IO ()
fillRect a (Rect x y w h) c = emitQuadUV a x y (x + w) (y + h) c 0 0 0 0

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

-- | Write one quad's four vertices; returns the first vertex's index.
{-# INLINE pushVertices #-}
pushVertices :: DrawArena -> Float -> Float -> Float -> Float -> Color -> Float -> Float -> Float -> Float -> IO Word32
pushVertices a !x0 !y0 !x1 !y1 col !u0 !v0 !u1 !v1 = do
  n <- readURef (daVertexCount a)
  growBuffer (daVertex a) (daVertexPtr a) (daVertexCap a) ((n + 4) * vertexSize)
  buf <- readIORef (daVertexPtr a)
  writeQuadVertices buf n x0 y0 x1 y1 col u0 v0 u1 v1
  writeURef (daVertexCount a) (n + 4)
  pure (fromIntegral n)

-- | The two triangles of a quad, as six indices over its four vertices.
{-# INLINE pushIndices #-}
pushIndices :: DrawArena -> Word32 -> IO ()
pushIndices a !base = do
  n <- readURef (daIndexCount a)
  growBuffer (daIndex a) (daIndexPtr a) (daIndexCap a) ((n + 6) * indexSize)
  buf <- readIORef (daIndexPtr a)
  writeQuadIndices buf n base
  writeURef (daIndexCount a) (n + 6)

-- | Extend the open batch when the quad's clip and texture continue it;
-- else close the open batch into the command list and open a fresh one.
-- Continuing a batch only bumps a counter, so the common run of quads
-- under one clip and texture allocates nothing per quad.
{-# INLINE batchCommand #-}
batchCommand :: DrawArena -> Rect -> IO ()
batchCommand a clip = do
  texture <- readIORef (daLastTexture a)
  idxCount <- readURef (daIndexCount a)
  let idxStart = idxCount - 6
  openClip <- readIORef (daBatchClip a)
  openTex <- readIORef (daBatchTexture a)
  openCount <- readURef (daBatchCount a)
  if openCount > 0 && openClip == clip && openTex == texture
    then writeURef (daBatchCount a) (openCount + 6)
    else do
      when (openCount > 0) $ do
        openStart <- readURef (daBatchStart a)
        modifyIORef'
          (daCommands a)
          ( DrawCmd
              (rectX openClip)
              (rectY openClip)
              (rectW openClip)
              (rectH openClip)
              openTex
              (fromIntegral openStart)
              (fromIntegral openCount)
              :
          )
      writeIORef (daBatchClip a) clip
      writeIORef (daBatchTexture a) texture
      writeURef (daBatchStart a) idxStart
      writeURef (daBatchCount a) 6

-- | Write four vertices in the C renderer's 32-byte layout: position, RGBA
-- as four floats, UV. Colour channels come from the packed @0xRRGGBBAA@ word.
writeQuadVertices :: Ptr Word8 -> Int -> Float -> Float -> Float -> Float -> Color -> Float -> Float -> Float -> Float -> IO ()
writeQuadVertices !buf !n !x0 !y0 !x1 !y1 col !u0 !v0 !u1 !v1 = do
  let !base = buf `plusPtr` (n * vertexSize)
      !r = fromIntegral (colorR col) / 255
      !g = fromIntegral (colorG col) / 255
      !b = fromIntegral (colorB col) / 255
      !a = fromIntegral (colorA col) / 255
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

-- | Write the quad's six indices.
writeQuadIndices :: Ptr Word32 -> Int -> Word32 -> IO ()
writeQuadIndices !buf !n !base = do
  let !p = buf `plusPtr` (n * indexSize)
      !i0 = base
      !i1 = base + 1
      !i2 = base + 2
      !i3 = base + 3
  pokeByteOff p 0 i0
  pokeByteOff p 4 i1
  pokeByteOff p 8 i2
  pokeByteOff p 12 i0
  pokeByteOff p 16 i2
  pokeByteOff p 20 i3

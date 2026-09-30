-- | The draw list: widgets emit quads into growable vertex and index
-- buffers, batched into commands that share a clip rectangle and a texture.
-- The vertex layout is nano-ui RGFW's 32-byte quad: logical position, RGBA
-- as four floats, and UV, so the C renderer needs no adaptation beyond its
-- name. Texture ids: 0 is flat geometry, 1 the glyph atlas, and 2 or more
-- are backend-registered images.
module ChibiUI.Internal.Draw
  ( DrawArena
  , DrawCmd (..)
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

import Data.IORef
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
  , daVertexCap :: !(IORef Int)
  , daVertexCount :: !(IORef Int)
  , daIndex :: !(IORef (ForeignPtr Word8))
  , daIndexCap :: !(IORef Int)
  , daIndexCount :: !(IORef Int)
  , daCommands :: !(IORef [DrawCmd])
  , daLastClip :: !(IORef Rect)
  , daLastTexture :: !(IORef Int)
  , daClipStack :: !(IORef [Rect])
  }

-- | A brand-new arena.
newDrawArena :: IO DrawArena
newDrawArena = do
  vbuf <- newBuffer 0
  ibuf <- newBuffer 0
  vref <- newIORef vbuf
  iref <- newIORef ibuf
  cref <- newIORef []
  lcref <- newIORef infiniteClip
  ltref <- newIORef texFlat
  cstack <- newIORef []
  vcap <- newIORef 0
  icap <- newIORef 0
  vcnt <- newIORef 0
  icnt <- newIORef 0
  pure
    DrawArena
      { daVertex = vref
      , daVertexCap = vcap
      , daVertexCount = vcnt
      , daIndex = iref
      , daIndexCap = icap
      , daIndexCount = icnt
      , daCommands = cref
      , daLastClip = lcref
      , daLastTexture = ltref
      , daClipStack = cstack
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
  writeIORef (daVertexCount a) 0
  writeIORef (daIndexCount a) 0
  writeIORef (daCommands a) []
  writeIORef (daLastClip a) infiniteClip
  writeIORef (daLastTexture a) texFlat
  writeIORef (daClipStack a) []

-- | Snapshot the frame. The arena must not be reset and emitted into again
-- until the backend has rendered or copied the snapshot.
finishFrame :: DrawArena -> IO DrawData
finishFrame a = do
  vfp <- readIORef (daVertex a)
  vc <- readIORef (daVertexCount a)
  ifp <- readIORef (daIndex a)
  ic <- readIORef (daIndexCount a)
  cmds <- reverse <$> readIORef (daCommands a)
  pure
    DrawData
      { drawVertices = vfp
      , drawVertexCount = vc
      , drawIndices = ifp
      , drawIndexCount = ic
      , drawCommands = cmds
      }

-- | Grow a buffer to at least @need@ bytes, keeping the old contents.
growBuffer :: IORef (ForeignPtr Word8) -> IORef Int -> Int -> IO (ForeignPtr Word8)
growBuffer ref capRef need = do
  fp <- readIORef ref
  cap <- readIORef capRef
  if need <= cap
    then pure fp
    else do
      let cap' = max need (max 4096 (cap * 2))
      fp' <- newBuffer cap'
      withForeignPtr fp $ \p ->
        withForeignPtr fp' $ \p' -> copyBytes p' p cap
      writeIORef ref fp'
      writeIORef capRef cap'
      pure fp'

-- | The current clip, in window coordinates. Emitters cut every quad to it.
currentClip :: DrawArena -> IO Rect
currentClip a = readIORef (daLastClip a)

-- | Push @r@ intersected with the current clip. Every command emitted until
-- the matching 'popClip' is cut to the result.
pushClip :: DrawArena -> Rect -> IO ()
pushClip a r = do
  cur <- readIORef (daLastClip a)
  modifyIORef' (daClipStack a) (cur :)
  let clipped = maybe (Rect 0 0 0 0) id (rectIntersect cur r)
  writeIORef (daLastClip a) clipped

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
pushVertices :: DrawArena -> Float -> Float -> Float -> Float -> Color -> Float -> Float -> Float -> Float -> IO Word32
pushVertices a !x0 !y0 !x1 !y1 col !u0 !v0 !u1 !v1 = do
  n <- readIORef (daVertexCount a)
  fp <- growBuffer (daVertex a) (daVertexCap a) ((n + 4) * vertexSize)
  withForeignPtr fp $ \buf -> writeQuadVertices buf n x0 y0 x1 y1 col u0 v0 u1 v1
  writeIORef (daVertexCount a) (n + 4)
  pure (fromIntegral n)

-- | The two triangles of a quad, as six indices over its four vertices.
pushIndices :: DrawArena -> Word32 -> IO ()
pushIndices a !base = do
  n <- readIORef (daIndexCount a)
  fp <- growBuffer (daIndex a) (daIndexCap a) ((n + 6) * indexSize)
  withForeignPtr fp $ \buf -> writeQuadIndices (castPtr buf :: Ptr Word32) n base
  writeIORef (daIndexCount a) (n + 6)

-- | Extend the last command when the batch's clip, texture and index range
-- continue it; else start a new one.
batchCommand :: DrawArena -> Rect -> IO ()
batchCommand a clip = do
  texture <- readIORef (daLastTexture a)
  idxCount <- readIORef (daIndexCount a)
  cmds <- readIORef (daCommands a)
  let idxStart = fromIntegral (idxCount - 6) :: Word32
  case cmds of
    (cmd : rest)
      | cmdClipX cmd == rectX clip
          && cmdClipY cmd == rectY clip
          && cmdClipW cmd == rectW clip
          && cmdClipH cmd == rectH clip
          && cmdTextureId cmd == texture
          && cmdIndexOffset cmd + cmdIndexCount cmd == idxStart ->
          writeIORef (daCommands a) (cmd {cmdIndexCount = cmdIndexCount cmd + 6} : rest)
    _ -> writeIORef (daCommands a) (DrawCmd (rectX clip) (rectY clip) (rectW clip) (rectH clip) texture idxStart 6 : cmds)

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

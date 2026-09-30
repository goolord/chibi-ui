-- | Rudimentary frame damage: what changed between one frame's draw list
-- and the previous one, as a handful of rectangles in logical pixels.
--
-- The comparison is per quad, not per widget. Quads are emitted in a
-- canonical layout (four 32-byte vertices, six indices, in order), so the
-- k-th quad of one frame lines up with the k-th quad of the next; inserted
-- or removed content shifts the tail, which simply reads as more damage.
-- A frame whose vertices, quad count, and batch texture sequence all
-- match the previous one is damage-free: it renders to the same pixels, so
-- the backend can skip it and idle. A few changed quads become damage
-- rectangles; anything bigger or structurally ambiguous is a full frame.
module ChibiUI.Internal.Damage
  ( Damage (..)
  , FrameSnapshot
  , takeSnapshot
  , frameDamage
  , trackFrame
  , commandQuadBounds
  ) where

import Data.IORef (IORef, readIORef, writeIORef)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Unsafe as BSU
import Data.Word (Word8)
import Foreign.C.Types (CInt (..), CSize (..))
import Foreign.ForeignPtr (withForeignPtr)
import Foreign.Ptr (Ptr, castPtr, plusPtr)
import Foreign.Storable (peekByteOff)
import ChibiUI.Internal.Draw (DrawCmd (..), DrawData (..), vertexSize)
import ChibiUI.Internal.Types
  ( Rect (..)
  , Size (..)
  , foldUpTo
  , rectArea
  , rectNonEmpty
  , rectsOverlap
  , rectUnion
  )

foreign import ccall unsafe "string.h memcmp"
  c_memcmp :: Ptr Word8 -> Ptr Word8 -> CSize -> IO CInt

-- | What a frame owes the screen.
data Damage
  = DamageNone
  -- ^ The frame's geometry is byte-identical to the previous one; nothing
    -- needs drawing.
  | DamageRects [Rect]
  -- ^ Repaint these logical-pixel rectangles (and the commands intersecting
    -- them) into the retained framebuffer.
  | DamageFull
  -- ^ Repaint everything: the frames differ structurally, or too much of
    -- the window changed to track.
  deriving (Eq, Show)

-- | One frame's copied geometry, for diffing against the next frame. The
-- arena is reset and reused every frame, so a snapshot owns its bytes.
data FrameSnapshot = FrameSnapshot
  { snapVertices :: !BS.ByteString
  -- ^ The used prefix of the vertex buffer: quad @k@ lives at byte
    -- @k * 4 * vertexSize@.
  , snapTextures :: ![Int]
  -- ^ Each batch's texture, in order. Index ranges are not kept: they
    -- shift when quads are inserted or removed, which the quad diff
    -- already sees; a texture changing in place can repaint different
    -- pixels over identical geometry and forces a full frame.
  }

-- | Bytes per quad: four vertices.
quadBytes :: Int
quadBytes = 4 * vertexSize

-- | Quads a snapshot holds.
snapQuadCount :: FrameSnapshot -> Int
snapQuadCount snap = BS.length (snapVertices snap) `div` quadBytes

-- | Quads whose diff alone is worth reporting before falling back to a
-- full frame.
maxChangedQuads :: Int
maxChangedQuads = 64

-- | Damage rectangles kept separately before collapsing to their union.
maxDamageRects :: Int
maxDamageRects = 8

-- | Fraction of the window covered that makes a union a full frame.
damageFullFrac :: Float
damageFullFrac = 0.7

-- | Copy a frame's geometry into an owned snapshot.
takeSnapshot :: DrawData -> IO FrameSnapshot
takeSnapshot dd = do
  verts <- copyVertices dd
  pure
    FrameSnapshot
      { snapVertices = verts
      , snapTextures = map cmdTextureId (drawCommands dd)
      }

-- | Diff a snapshot against the frame just drawn. Texture contents are
-- assumed unchanged; the backend forces a full frame when the atlas or an
-- image uploads. The frame is compared against the arena in place, so the
-- diff itself copies nothing.
frameDamage :: FrameSnapshot -> DrawData -> Size -> IO Damage
frameDamage snap dd window = do
  same <- vertexBytesEq (snapVertices snap) dd
  let old = snapTextures snap
      new = map cmdTextureId (drawCommands dd)
  if same && old == new
    then pure DamageNone
    else
      -- The same batch structure over different textures.
      if old /= new && length old == length new
        then pure DamageFull
        else withForeignPtr (drawVertices dd) $ \vp -> do
          let oldN = snapQuadCount snap
              newN = drawVertexCount dd `div` 4
          rects <- changedQuadRects (snapVertices snap) vp (min oldN newN) oldN newN
          pure (maybe DamageFull (mergeDamage window) rects)

-- | Whether the frame's used vertex prefix equals the snapshot's bytes,
-- compared in place: the arena's buffer against the snapshot's copy.
vertexBytesEq :: BS.ByteString -> DrawData -> IO Bool
vertexBytesEq snap dd = do
  let len = drawVertexCount dd * vertexSize
  if len /= BS.length snap
    then pure False
    else
      withForeignPtr (drawVertices dd) $ \vp ->
        BSU.unsafeUseAsCStringLen snap $ \(sp, _) ->
          (== 0) <$> c_memcmp vp (castPtr sp) (fromIntegral len)

-- | Copy the frame's used vertex prefix into an owned bytestring.
copyVertices :: DrawData -> IO BS.ByteString
copyVertices dd =
  withForeignPtr (drawVertices dd) $ \p ->
    BS.packCStringLen (castPtr p, drawVertexCount dd * vertexSize)

-- | Diff and update the snapshot a backend keeps of the frame on screen.
-- A damage-free frame leaves the stored snapshot in place: it still
-- describes the current frame exactly.
trackFrame :: IORef (Maybe FrameSnapshot) -> Size -> DrawData -> IO Damage
trackFrame ref window dd = do
  prev <- readIORef ref
  damage <- maybe (pure DamageFull) (\snap -> frameDamage snap dd window) prev
  case damage of
    DamageNone -> pure ()
    _ -> takeSnapshot dd >>= writeIORef ref . Just
  pure damage

-- | Bounds of every quad that differs between the snapshot's vertices and
-- the frame's at @new@ over @[0, n)@, plus the tail quads present in only
-- one of them, from both sides. 'Nothing' when there is too much to track.
changedQuadRects :: BS.ByteString -> Ptr Word8 -> Int -> Int -> Int -> IO (Maybe [Rect])
changedQuadRects old new n oldN newN =
  BSU.unsafeUseAsCString old $ \op -> do
    let oldP = castPtr op
    diff <- diffQuads oldP new n
    case diff of
      Nothing -> pure Nothing
      Just changed
        | length changed + (oldN - n) + (newN - n) > maxChangedQuads -> pure Nothing
        | otherwise -> do
            let touched = changed ++ [n .. oldN - 1] ++ [n .. newN - 1]
            -- Both sides of every changed quad: the old area may need
            -- clearing even where the new frame draws nothing.
            oldSide <- mapM (quadAtPtr oldP) [k | k <- touched, k < oldN]
            newSide <- mapM (quadAtPtr new) [k | k <- touched, k < newN]
            pure (Just (filter rectNonEmpty (oldSide ++ newSide)))

-- | Indices of the quads over @[0, n)@ whose bytes differ, or 'Nothing'
-- past the tracking budget.
diffQuads :: Ptr Word8 -> Ptr Word8 -> Int -> IO (Maybe [Int])
diffQuads old new n = go 0 (0 :: Int) []
  where
    go !k !count acc
      | count > maxChangedQuads = pure Nothing
      | k >= n = pure (Just (reverse acc))
      | otherwise = do
          let off = k * quadBytes
          d <- c_memcmp (old `plusPtr` off) (new `plusPtr` off) (fromIntegral quadBytes)
          if d == 0 then go (k + 1) count acc else go (k + 1) (count + 1) (k : acc)

-- | Union the bounds of the quads a command's index range covers, for
-- testing a command against a damage rectangle without drawing it.
commandQuadBounds :: DrawData -> DrawCmd -> IO Rect
commandQuadBounds dd cmd =
  withForeignPtr (drawVertices dd) $ \vp -> do
    let first = fromIntegral (cmdIndexOffset cmd) `div` 6
        end = (fromIntegral (cmdIndexOffset cmd) + fromIntegral (cmdIndexCount cmd)) `div` 6
    if end <= first
      then pure (Rect 0 0 0 0)
      else do
        seed <- quadAtPtr vp first
        foldUpTo (end - first - 1) (\acc i -> rectUnion acc <$> quadAtPtr vp (first + 1 + i)) seed

-- | The bounds of quad @k@: the min and max of vertex 0's and vertex 2's
-- corners in the first 8 bytes of each vertex.
quadAtPtr :: Ptr Word8 -> Int -> IO Rect
quadAtPtr p !k = do
  let base = p `plusPtr` (k * quadBytes)
  x0 <- peekByteOff base 0 :: IO Float
  y0 <- peekByteOff base 4 :: IO Float
  x1 <- peekByteOff base (2 * vertexSize) :: IO Float
  y1 <- peekByteOff base (2 * vertexSize + 4) :: IO Float
  pure (Rect (min x0 x1) (min y0 y1) (abs (x1 - x0)) (abs (y1 - y0)))

-- | Merge overlapping rectangles, collapse runs of many into their union,
-- and give up on a full frame when the union covers most of the window.
mergeDamage :: Size -> [Rect] -> Damage
mergeDamage window rs0
  | null merged = DamageNone
  | rectArea union / max 1 (sizeW window * sizeH window) >= damageFullFrac = DamageFull
  | length merged > maxDamageRects = DamageRects [union]
  | otherwise = DamageRects merged
  where
    merged = mergeRects (filter rectNonEmpty rs0)
    union = foldr1 rectUnion merged

-- | One merging pass: each rectangle absorbs every later one it overlaps,
-- growing as it does, before the pass moves on. A kept rectangle can
-- still overlap one that grew after it; the damage stays covered either
-- way.
mergeRects :: [Rect] -> [Rect]
mergeRects [] = []
mergeRects (r : rs) = case takeIntersecting r rs of
  Just (hit, rest) -> mergeRects (rectUnion r hit : rest)
  Nothing -> r : mergeRects rs

-- | The first rectangle overlapping @r@, and the others without it.
takeIntersecting :: Rect -> [Rect] -> Maybe (Rect, [Rect])
takeIntersecting r = go []
  where
    go _ [] = Nothing
    go skipped (x : rest)
      | rectsOverlap r x = Just (x, reverse skipped ++ rest)
      | otherwise = go (x : skipped) rest

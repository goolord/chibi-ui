-- | Rudimentary frame damage: what changed between one frame's draw list
-- and the previous one, as a handful of rectangles in logical pixels.
--
-- The comparison is per quad, not per widget. Quads are emitted in a
-- canonical layout (four 32-byte vertices, in order), so the k-th quad of
-- one frame lines up with the k-th quad of the next; inserted or removed
-- content shifts the tail, which simply reads as more damage. A frame
-- whose vertices, quad count, and batch texture sequence all match the
-- previous one is damage-free: it renders to the same pixels, so the
-- backend can skip it and idle. A few changed quads become damage
-- rectangles; anything bigger or structurally ambiguous is a full frame.
module ChibiUI.Internal.Damage
  ( Damage (..)
  , Upload (..)
  , FrameSnapshot
  , takeSnapshot
  , frameDamage
  , trackFrame
  , trackUploads
  ) where

import Data.IORef (IORef, readIORef, writeIORef)
import Data.Word (Word8)
import Foreign.C.Types (CInt (..), CSize (..))
import Foreign.ForeignPtr (ForeignPtr, mallocForeignPtrBytes, withForeignPtr)
import Foreign.Marshal.Utils (copyBytes)
import Foreign.Ptr (Ptr, plusPtr)
import Foreign.Storable (peekByteOff)
import ChibiUI.Internal.Draw (DrawCmd (..), DrawData (..), quadBytes, vertexSize)
import ChibiUI.Internal.Types
  ( Rect (..)
  , Size (..)
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
  -- ^ Nothing visible changed from the previous frame; nothing needs
    -- drawing.
  | DamageRects [Rect]
  -- ^ Repaint these logical-pixel rectangles (and the commands intersecting
    -- them) into the retained framebuffer.
  | DamageFull
  -- ^ Repaint everything: the frames differ structurally, or too much of
    -- the window changed to track.
  deriving (Eq, Show)

-- | What a renderer holding the tracked frame's vertices must upload of
-- the next one.
data Upload
  = UploadNone
  -- ^ Its copy already draws the frame.
  | UploadQuads [Int]
  -- ^ Just these quads, in ascending order; the quad count held.
  | UploadAll
  deriving (Eq, Show)

-- | One frame's copied geometry, for diffing against the next frame. The
-- arena is reset and reused every frame, so a snapshot owns its bytes.
-- 'trackFrame' reuses the buffer of the snapshot it replaces.
data FrameSnapshot = FrameSnapshot
  { snapBuffer :: !(ForeignPtr Word8)
  -- ^ The used prefix of the frame's vertex buffer, in a buffer of
    -- 'snapCapacity' bytes: quad @k@ lives at byte @k * quadBytes@.
  , snapCapacity :: !Int
  , snapBytes :: !Int
  , snapTextures :: ![Int]
  -- ^ Each batch's texture, in order. Index ranges are not kept: they
    -- shift when quads are inserted or removed, which the quad diff
    -- already sees; a texture changing in place can repaint different
    -- pixels over identical geometry and forces a full frame.
  }

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
takeSnapshot = copyInto Nothing

-- | Copy a frame's geometry into a snapshot, reusing an old snapshot's
-- buffer when it is large enough.
copyInto :: Maybe FrameSnapshot -> DrawData -> IO FrameSnapshot
copyInto old dd = do
  let len = drawVertexCount dd * vertexSize
  (buf, cap) <- case old of
    Just snap | snapCapacity snap >= len -> pure (snapBuffer snap, snapCapacity snap)
    _ -> do
      let cap = max len (maybe 0 ((* 2) . snapCapacity) old)
      fp <- mallocForeignPtrBytes (max 1 cap)
      pure (fp, cap)
  withForeignPtr buf $ \dst ->
    withForeignPtr (drawVertices dd) $ \src -> copyBytes dst src len
  pure
    FrameSnapshot
      { snapBuffer = buf
      , snapCapacity = cap
      , snapBytes = len
      , snapTextures = map cmdTextureId (drawCommands dd)
      }

-- | Diff a snapshot against the frame just drawn. Texture contents are
-- assumed unchanged; the backend forces a full frame when the atlas or an
-- image uploads. The frame is compared against the arena in place, so the
-- diff itself copies nothing.
frameDamage :: FrameSnapshot -> DrawData -> Size -> IO Damage
frameDamage snap dd window = fst <$> diffFrame snap dd window

-- | The damage, and the quads that differ from the snapshot's.
diffFrame :: FrameSnapshot -> DrawData -> Size -> IO (Damage, Upload)
diffFrame snap dd window =
  withForeignPtr (snapBuffer snap) $ \old ->
    withForeignPtr (drawVertices dd) $ \new -> do
      same <-
        if len /= snapBytes snap
          then pure False
          else (== 0) <$> c_memcmp old new (fromIntegral len)
      compareWith same old new
  where
    len = drawVertexCount dd * vertexSize
    oldTex = snapTextures snap
    newTex = map cmdTextureId (drawCommands dd)
    oldN = snapBytes snap `div` quadBytes
    newN = drawVertexCount dd `div` 4
    compareWith same old new
      | same && oldTex == newTex = pure (DamageNone, UploadNone)
      -- The same batch structure over different textures.
      | oldTex /= newTex && length oldTex == length newTex = pure (DamageFull, UploadAll)
      | otherwise = do
          diff <- changedQuads old new oldN newN
          pure $ case diff of
            Nothing -> (DamageFull, UploadAll)
            Just (changed, rects) ->
              (mergeDamage window rects, if oldN == newN then UploadQuads changed else UploadAll)

-- | Diff and update the snapshot a backend keeps of the frame on screen.
trackFrame :: IORef (Maybe FrameSnapshot) -> Size -> DrawData -> IO Damage
trackFrame ref window dd = fst <$> trackUploads ref window dd

-- | 'trackFrame', with what a renderer that keeps the tracked frame's
-- vertices must upload of this one. A damage-free frame keeps the stored
-- bytes and uploads nothing: they still draw the current frame exactly.
trackUploads :: IORef (Maybe FrameSnapshot) -> Size -> DrawData -> IO (Damage, Upload)
trackUploads ref window dd = do
  prev <- readIORef ref
  case prev of
    Nothing -> (DamageFull, UploadAll) <$ (writeIORef ref . Just =<< copyInto Nothing dd)
    Just snap -> do
      diff@(damage, _) <- diffFrame snap dd window
      if damage == DamageNone
        then pure (DamageNone, UploadNone)
        else diff <$ (writeIORef ref . Just =<< copyInto prev dd)

-- | The quads that differ between the old vertices and the new over the
-- quads both have, with the bounds of every quad touched: those, plus the
-- tail quads present in only one of them, from both sides. 'Nothing' when
-- there is too much to track.
changedQuads :: Ptr Word8 -> Ptr Word8 -> Int -> Int -> IO (Maybe ([Int], [Rect]))
changedQuads old new oldN newN = do
  let n = min oldN newN
  diff <- diffQuads old new n (maxChangedQuads - (oldN - n) - (newN - n))
  case diff of
    Nothing -> pure Nothing
    Just changed -> do
      -- Both sides of every changed quad: the old area may need
      -- clearing even where the new frame draws nothing.
      oldSide <- mapM (quadAtPtr old) (changed ++ [n .. oldN - 1])
      newSide <- mapM (quadAtPtr new) (changed ++ [n .. newN - 1])
      pure (Just (changed, oldSide ++ newSide))

-- | Indices of the quads over @[0, n)@ whose bytes differ, or 'Nothing'
-- past @budget@ of them.
diffQuads :: Ptr Word8 -> Ptr Word8 -> Int -> Int -> IO (Maybe [Int])
diffQuads old new n budget = go 0 (0 :: Int) []
  where
    go !k !count acc
      | count > budget = pure Nothing
      | k >= n = pure (Just (reverse acc))
      | otherwise = do
          let off = k * quadBytes
          d <- c_memcmp (old `plusPtr` off) (new `plusPtr` off) (fromIntegral quadBytes)
          if d == 0 then go (k + 1) count acc else go (k + 1) (count + 1) (k : acc)

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
mergeRects (r : rs) = case break (rectsOverlap r) rs of
  (before, hit : after) -> mergeRects (rectUnion r hit : before ++ after)
  _ -> r : mergeRects rs

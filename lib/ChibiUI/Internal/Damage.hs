-- | Rudimentary frame damage: what changed between one frame's draw list
-- and the previous one, as a handful of rectangles in logical pixels.
--
-- The comparison is per quad, not per widget. Quads are emitted in a
-- canonical layout (four 32-byte vertices, six indices, in order), so the
-- k-th quad of one frame lines up with the k-th quad of the next; inserted
-- or removed content shifts the tail, which simply reads as more damage.
-- A frame whose vertices, quad count, and batch clip\/texture sequence all
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
import Foreign.ForeignPtr (withForeignPtr)
import Foreign.Ptr (Ptr, castPtr, plusPtr)
import Foreign.Storable (peekByteOff)
import ChibiUI.Internal.Draw (DrawCmd (..), DrawData (..), vertexSize)
import ChibiUI.Internal.Types
  ( Rect (..)
  , Size (..)
  , rectArea
  , rectIntersect
  , rectNonEmpty
  , rectUnion
  )

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
  , snapQuadCount :: !Int
  , snapBatches :: ![(Rect, Int)]
  -- ^ Each command's clip and texture, in order, without the index ranges.
    -- Ranges shift when quads are inserted or removed, which the quad diff
    -- already sees; a clip or texture changing in place can repaint
    -- different pixels over identical geometry and forces a full frame.
  }

-- | Bytes per quad: four vertices.
quadBytes :: Int
quadBytes = 4 * vertexSize

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
  verts <-
    withForeignPtr (drawVertices dd) $ \p ->
      BS.packCStringLen (castPtr p, drawVertexCount dd * vertexSize)
  pure
    FrameSnapshot
      { snapVertices = verts
      , snapQuadCount = drawIndexCount dd `div` 6
      , snapBatches = [(cmdRect c, cmdTextureId c) | c <- drawCommands dd]
      }

-- | Diff a snapshot against the frame just drawn. Texture contents are
-- assumed unchanged; the backend forces a full frame when the atlas or an
-- image uploads.
frameDamage :: FrameSnapshot -> DrawData -> Size -> IO Damage
frameDamage snap dd window = do
  let newQuads = drawIndexCount dd `div` 6
      newBatches = [(cmdRect c, cmdTextureId c) | c <- drawCommands dd]
  newVerts <-
    withForeignPtr (drawVertices dd) $ \p ->
      BS.packCStringLen (castPtr p, drawVertexCount dd * vertexSize)
  if newQuads == snapQuadCount snap && newBatches == snapBatches snap && newVerts == snapVertices snap
    then pure DamageNone
    else
      if length newBatches == length (snapBatches snap)
        && or (zipWith (/=) newBatches (snapBatches snap))
        then pure DamageFull
        else do
          let oldN = snapQuadCount snap
              n = min oldN newQuads
          rects <- changedQuadRects (snapVertices snap) newVerts n oldN newQuads
          pure (maybe DamageFull (mergeDamage window) rects)

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

-- | Bounds of every quad that differs between two vertex buffers over
-- @[0, n)@, plus the tail quads present in only one of them, from both
-- sides. 'Nothing' when there is too much to track.
changedQuadRects :: BS.ByteString -> BS.ByteString -> Int -> Int -> Int -> IO (Maybe [Rect])
changedQuadRects old new n oldN newN = case diffQuads old new n of
  Nothing -> pure Nothing
  Just changed -> do
    let touched = changed ++ [n .. oldN - 1] ++ [n .. newN - 1]
    if length touched > maxChangedQuads
      then pure Nothing
      else do
        -- Both sides of every changed quad: the old area may need clearing
        -- even where the new frame draws nothing.
        oldSide <- mapM (quadBounds old) [k | k <- touched, k < oldN]
        newSide <- mapM (quadBounds new) [k | k <- touched, k < newN]
        pure (Just (filter rectNonEmpty (oldSide ++ newSide)))

-- | Indices of the quads over @[0, n)@ whose bytes differ, or 'Nothing'
-- past the tracking budget.
diffQuads :: BS.ByteString -> BS.ByteString -> Int -> Maybe [Int]
diffQuads old new n = go 0 []
  where
    go !k acc
      | length acc > maxChangedQuads = Nothing
      | k >= n = Just (reverse acc)
      | quadEq old new k = go (k + 1) acc
      | otherwise = go (k + 1) (k : acc)

-- | Whether quad @k@ is byte-identical in both buffers.
quadEq :: BS.ByteString -> BS.ByteString -> Int -> Bool
quadEq a b !k = eq (k * quadBytes) 0
  where
    eq !base !i
      | i >= quadBytes = True
      | BSU.unsafeIndex a (base + i) /= BSU.unsafeIndex b (base + i) = False
      | otherwise = eq base (i + 1)

-- | A quad's axis-aligned bounds from vertex 0's and vertex 2's corners.
quadBounds :: BS.ByteString -> Int -> IO Rect
quadBounds bs !k = BSU.unsafeUseAsCString bs $ \p -> quadAtPtr (castPtr p) k

-- | Union the bounds of the quads a command's index range covers, for
-- testing a command against a damage rectangle without drawing it.
commandQuadBounds :: DrawData -> DrawCmd -> IO Rect
commandQuadBounds dd cmd =
  withForeignPtr (drawVertices dd) $ \vp -> do
    let first = fromIntegral (cmdIndexOffset cmd) `div` 6
        end = (fromIntegral (cmdIndexOffset cmd) + fromIntegral (cmdIndexCount cmd)) `div` 6
    qs <- mapM (quadAtPtr vp) [first .. end - 1]
    pure (foldr rectUnion (Rect 0 0 0 0) qs)

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
    merged = mergeAll (filter rectNonEmpty rs0)
    union = foldr1 rectUnion merged

-- | Repeatedly union any two overlapping rectangles until none overlap.
mergeAll :: [Rect] -> [Rect]
mergeAll rs = let (out, changed) = mergeStep rs in if changed then mergeAll out else out

-- | One merging pass over the rectangles.
mergeStep :: [Rect] -> ([Rect], Bool)
mergeStep [] = ([], False)
mergeStep (r : rs) = case takeIntersecting r rs of
  Just (hit, rest) -> mergeStep (rectUnion r hit : rest)
  Nothing -> let (out, changed) = mergeStep rs in (r : out, changed)

-- | The first rectangle overlapping @r@, and the others without it.
takeIntersecting :: Rect -> [Rect] -> Maybe (Rect, [Rect])
takeIntersecting r = go []
  where
    go _ [] = Nothing
    go skipped (x : rest) = case rectIntersect r x of
      Just _ -> Just (x, reverse skipped ++ rest)
      Nothing -> go (x : skipped) rest

-- | A command's clip as a rect.
cmdRect :: DrawCmd -> Rect
cmdRect c = Rect (cmdClipX c) (cmdClipY c) (cmdClipW c) (cmdClipH c)

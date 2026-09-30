{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE UnboxedTuples #-}

-- | A mutable open-addressing table from widget slot to rect: what the
-- context records each frame's widget geometry in, for hit tests.
-- Rebuilding an 'IntMap' every frame
-- pays a fresh path of nodes per widget and turns the whole last map into
-- garbage; this table writes each rect in place and clears by zeroing
-- keys, so steady frames allocate nothing. Keys are widget slots, which
-- are never zero, so an all-zero key array reads as empty.
module ChibiUI.Internal.RectTable
  ( RectTable
  , newRectTable
  , clearRectTable
  , insertRect
  , lookupRect
  , memberRect
  , rectTableToList
  ) where

import Control.Monad (forM_, when)
import Data.Bits ((.&.))
import Data.IORef
import GHC.Exts
  ( Float (..)
  , Int (..)
  , MutableByteArray#
  , RealWorld
  , isTrue#
  , newByteArray#
  , readFloatArray#
  , readIntArray#
  , setByteArray#
  , writeFloatArray#
  , writeIntArray#
  , (==#)
  , (+#)
  , (*#)
  )
import GHC.IO (IO (IO))
import ChibiUI.Internal.Types (Rect (..))
import ChibiUI.Internal.URef

-- | A lifted box for one mutable byte array, so growth can swap it inside
-- an 'IORef'.
data MBox = MBox !(MutableByteArray# RealWorld)

-- | The table: one 'Int' array of keys, one 'Float' array of four
-- coordinates per slot, a power-of-two capacity, and the live count.
data RectTable = RectTable
  { rtKeys :: !(IORef MBox)
  , rtRects :: !(IORef MBox)
  , rtCap :: !(IORef Int)
  , rtCount :: !URef
  }

-- | The starting capacity, a power of two like every capacity the table
-- grows through.
initialCap :: Int
initialCap = 256

-- | A new, empty table.
newRectTable :: IO RectTable
newRectTable = do
  keys <- allocKeys initialCap
  rects <- allocRects initialCap
  kref <- newIORef keys
  rref <- newIORef rects
  cap <- newIORef initialCap
  count <- newURef 0
  pure (RectTable kref rref cap count)

allocKeys :: Int -> IO MBox
allocKeys n@(I# n#) = do
  box <-
    IO $ \s -> case newByteArray# (n# *# 8#) s of (# s', arr #) -> (# s', MBox arr #)
  zeroKeys box n
  pure box

allocRects :: Int -> IO MBox
allocRects (I# n#) =
  IO $ \s -> case newByteArray# (n# *# 16#) s of (# s', arr #) -> (# s', MBox arr #)

-- | Zero every key, which empties the table. Coordinates go stale, but no
-- zero-keyed slot is ever read.
zeroKeys :: MBox -> Int -> IO ()
zeroKeys (MBox ba) (I# n#) =
  IO $ \s -> case setByteArray# ba 0# (n# *# 8#) 0# s of s' -> (# s', () #)

-- | Empty the table for reuse by the next frame.
clearRectTable :: RectTable -> IO ()
clearRectTable rt = do
  box <- readIORef (rtKeys rt)
  cap <- readIORef (rtCap rt)
  zeroKeys box cap
  writeURef (rtCount rt) 0

-- | Grow to keep the load under three quarters before inserting. Rehashes
-- the live entries into double the slots.
growTable :: RectTable -> IO ()
growTable rt = do
  cap <- readIORef (rtCap rt)
  let cap' = cap * 2
  keys' <- allocKeys cap'
  rects' <- allocRects cap'
  entries <- rectTableToList rt
  writeIORef (rtKeys rt) keys'
  writeIORef (rtRects rt) rects'
  writeIORef (rtCap rt) cap'
  writeURef (rtCount rt) 0
  forM_ entries $ \(k, r) -> insertRect rt k r

-- | Record a slot's rect, replacing any rect recorded under that slot
-- earlier in the same frame.
{-# INLINE insertRect #-}
insertRect :: RectTable -> Int -> Rect -> IO ()
insertRect rt k r = do
  count <- readURef (rtCount rt)
  cap <- readIORef (rtCap rt)
  when ((count + 1) * 4 > cap * 3) (growTable rt)
  i <- slotFor rt k
  fresh <- (== 0) <$> readKeyAt rt i
  writeRectAt rt i r
  when fresh $ do
    writeKeyAt rt i k
    writeURef (rtCount rt) (count + 1)

-- | The index of a slot's entry, or -1 when the slot was not recorded.
{-# INLINE probe #-}
probe :: RectTable -> Int -> IO Int
probe rt k = do
  i <- slotFor rt k
  key <- readKeyAt rt i
  pure (if key == 0 then -1 else i)

-- | Linear probing from the slot's home: the index holding the slot, or
-- the empty index where it would go. The load bound keeps an empty one.
{-# INLINE slotFor #-}
slotFor :: RectTable -> Int -> IO Int
slotFor rt k = do
  MBox keys <- readIORef (rtKeys rt)
  cap <- readIORef (rtCap rt)
  let !(I# k#) = k
      !(I# max#) = cap - 1
      !(I# start#) = k .&. (cap - 1)
      go i# s = case readIntArray# keys i# s of
        (# s', key# #)
          | isTrue# (key# ==# 0#) || isTrue# (key# ==# k#) -> (# s', I# i# #)
          | otherwise -> go (if isTrue# (i# ==# max#) then 0# else i# +# 1#) s'
  IO (go start#)

-- | The key stored at an entry index; zero for an empty entry.
{-# INLINE readKeyAt #-}
readKeyAt :: RectTable -> Int -> IO Int
readKeyAt rt (I# i#) = do
  MBox keys <- readIORef (rtKeys rt)
  IO $ \s -> case readIntArray# keys i# s of
    (# s', key# #) -> (# s', I# key# #)

{-# INLINE writeKeyAt #-}
writeKeyAt :: RectTable -> Int -> Int -> IO ()
writeKeyAt rt (I# i#) (I# k#) = do
  MBox keys <- readIORef (rtKeys rt)
  IO $ \s -> case writeIntArray# keys i# k# s of
    s' -> (# s', () #)

{-# INLINE writeRectAt #-}
writeRectAt :: RectTable -> Int -> Rect -> IO ()
writeRectAt rt (I# i#) (Rect (F# x) (F# y) (F# w) (F# h)) = do
  MBox rects <- readIORef (rtRects rt)
  let base# = i# *# 4#
  IO $ \s -> case writeFloatArray# rects base# x s of
    s1 -> case writeFloatArray# rects (base# +# 1#) y s1 of
      s2 -> case writeFloatArray# rects (base# +# 2#) w s2 of
        s3 -> case writeFloatArray# rects (base# +# 3#) h s3 of
          s4 -> (# s4, () #)

-- | The rect stored at an entry index.
{-# INLINE readRectAt #-}
readRectAt :: RectTable -> Int -> IO Rect
readRectAt rt (I# i#) = do
  MBox rects <- readIORef (rtRects rt)
  let base# = i# *# 4#
  IO $ \s -> case readFloatArray# rects base# s of
    (# s1, x #) -> case readFloatArray# rects (base# +# 1#) s1 of
      (# s2, y #) -> case readFloatArray# rects (base# +# 2#) s2 of
        (# s3, w #) -> case readFloatArray# rects (base# +# 3#) s3 of
          (# s4, h #) -> (# s4, Rect (F# x) (F# y) (F# w) (F# h) #)

-- | The rect recorded under a slot this frame, if any.
lookupRect :: RectTable -> Int -> IO (Maybe Rect)
lookupRect rt k = do
  i <- probe rt k
  if i < 0 then pure Nothing else Just <$> readRectAt rt i

-- | Whether a slot was recorded this frame.
memberRect :: RectTable -> Int -> IO Bool
memberRect rt k = (>= 0) <$> probe rt k

-- | Every recorded slot and rect, in slot order, for hosts and tests.
rectTableToList :: RectTable -> IO [(Int, Rect)]
rectTableToList rt = do
  cap <- readIORef (rtCap rt)
  let collect :: Int -> [(Int, Rect)] -> IO [(Int, Rect)]
      collect i acc
        | i < 0 = pure acc
        | otherwise = do
            key <- readKeyAt rt i
            if key == 0
              then collect (i - 1) acc
              else do
                r <- readRectAt rt i
                collect (i - 1) ((key, r) : acc)
  collect (cap - 1) []

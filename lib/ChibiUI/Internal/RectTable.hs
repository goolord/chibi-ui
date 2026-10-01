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
  , rectTableToList
  ) where

import Control.Monad (forM, forM_, when)
import Data.Bits ((.&.))
import Data.IORef
import Data.Maybe (catMaybes)
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
newRectTable =
  RectTable <$> (newIORef =<< allocKeys initialCap) <*> (newIORef =<< allocRects initialCap)
    <*> newIORef initialCap <*> newURef 0

allocKeys :: Int -> IO MBox
allocKeys n = do
  box <- newBox (n * 8)
  zeroKeys box n
  pure box

allocRects :: Int -> IO MBox
allocRects n = newBox (n * 16)

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
  keys <- readIORef (rtKeys rt)
  i <- slotFor rt keys k
  fresh <- (== 0) <$> readIntAt keys i
  writeRectAt rt i r
  when fresh (writeIntAt keys i k >> writeURef (rtCount rt) (count + 1))

-- | Linear probing from the slot's home: the index holding the slot, or
-- the empty index where it would go. The load bound keeps an empty one.
-- @keys@ is the table's current key box.
{-# INLINE slotFor #-}
slotFor :: RectTable -> MBox -> Int -> IO Int
slotFor rt (MBox keys) k = do
  cap <- readIORef (rtCap rt)
  let !(I# k#) = k
      !(I# max#) = cap - 1
      !(I# start#) = k .&. (cap - 1)
      go i# s = case readIntArray# keys i# s of
        (# s', key# #)
          | isTrue# (key# ==# 0#) || isTrue# (key# ==# k#) -> (# s', I# i# #)
          | otherwise -> go (if isTrue# (i# ==# max#) then 0# else i# +# 1#) s'
  IO (go start#)

{-# INLINE writeRectAt #-}
writeRectAt :: RectTable -> Int -> Rect -> IO ()
writeRectAt rt i (Rect x y w h) = do
  rects <- readIORef (rtRects rt)
  let put k = writeFloatAt rects (i * 4 + k)
  put 0 x >> put 1 y >> put 2 w >> put 3 h

-- | The rect stored at an entry index.
{-# INLINE readRectAt #-}
readRectAt :: RectTable -> Int -> IO Rect
readRectAt rt i = do
  rects <- readIORef (rtRects rt)
  let get k = readFloatAt rects (i * 4 + k)
  Rect <$> get 0 <*> get 1 <*> get 2 <*> get 3

-- The array primitives the table is built from, indexed by element.

{-# INLINE newBox #-}
newBox :: Int -> IO MBox
newBox (I# bytes#) = IO $ \s -> case newByteArray# bytes# s of (# s', arr #) -> (# s', MBox arr #)

{-# INLINE readIntAt #-}
readIntAt :: MBox -> Int -> IO Int
readIntAt (MBox a) (I# i#) = IO $ \s -> case readIntArray# a i# s of (# s', n# #) -> (# s', I# n# #)

{-# INLINE writeIntAt #-}
writeIntAt :: MBox -> Int -> Int -> IO ()
writeIntAt (MBox a) (I# i#) (I# n#) = IO $ \s -> (# writeIntArray# a i# n# s, () #)

{-# INLINE readFloatAt #-}
readFloatAt :: MBox -> Int -> IO Float
readFloatAt (MBox a) (I# i#) = IO $ \s -> case readFloatArray# a i# s of (# s', f# #) -> (# s', F# f# #)

{-# INLINE writeFloatAt #-}
writeFloatAt :: MBox -> Int -> Float -> IO ()
writeFloatAt (MBox a) (I# i#) (F# f#) = IO $ \s -> (# writeFloatArray# a i# f# s, () #)

-- | The rect recorded under a slot this frame, if any.
lookupRect :: RectTable -> Int -> IO (Maybe Rect)
lookupRect rt k = do
  keys <- readIORef (rtKeys rt)
  i <- slotFor rt keys k
  key <- readIntAt keys i
  if key == 0 then pure Nothing else Just <$> readRectAt rt i

-- | Every recorded slot and rect, in slot order, for hosts and tests.
rectTableToList :: RectTable -> IO [(Int, Rect)]
rectTableToList rt = do
  cap <- readIORef (rtCap rt)
  keys <- readIORef (rtKeys rt)
  fmap catMaybes . forM [0 .. cap - 1] $ \i -> do
    key <- readIntAt keys i
    if key == 0 then pure Nothing else Just . (,) key <$> readRectAt rt i

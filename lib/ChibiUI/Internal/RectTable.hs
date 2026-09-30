{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE UnboxedTuples #-}

-- | A mutable open-addressing table from widget slot to rect: what the
-- context records each frame's widget geometry in, and what it keeps of
-- the previous frame's for hit tests. Rebuilding an 'IntMap' every frame
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
  , Float#
  , Int (..)
  , Int#
  , MutableByteArray#
  , RealWorld
  , State#
  , andI#
  , isTrue#
  , newByteArray#
  , readFloatArray#
  , readIntArray#
  , writeFloatArray#
  , writeIntArray#
  , (==#)
  , (+#)
  , (-#)
  , (*#)
  )
import GHC.IO (IO (IO))
import ChibiUI.Internal.Types (Rect (..))

-- | A lifted box for one mutable byte array, so growth can swap it inside
-- an 'IORef'.
data MBox = MBox !(MutableByteArray# RealWorld)

-- | One unboxed mutable 'Int' cell, as in "ChibiUI.Internal.Draw": the
-- entry counter must update without boxing.
data URef = URef !(MutableByteArray# RealWorld)

{-# INLINE newURef #-}
newURef :: Int -> IO URef
newURef (I# n#) =
  IO $ \s -> case newByteArray# 8# s of
    (# s', cell #) -> case writeIntArray# cell 0# n# s' of
      s'' -> (# s'', URef cell #)

{-# INLINE readURef #-}
readURef :: URef -> IO Int
readURef (URef cell) =
  IO $ \s -> case readIntArray# cell 0# s of
    (# s', n# #) -> (# s', I# n# #)

{-# INLINE writeURef #-}
writeURef :: URef -> Int -> IO ()
writeURef (URef cell) (I# n#) =
  IO $ \s -> case writeIntArray# cell 0# n# s of
    s' -> (# s', () #)

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
zeroKeys (MBox ba) (I# n#) = IO (go 0#)
  where
    go !i# s
      | isTrue# (i# ==# n#) = (# s, () #)
      | otherwise = case writeIntArray# ba i# 0# s of
          s' -> go (i# +# 1#) s'

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
insertRect rt k (Rect (F# x) (F# y) (F# w) (F# h)) = do
  count <- readURef (rtCount rt)
  cap <- readIORef (rtCap rt)
  when ((count + 1) * 4 > cap * 3) (growTable rt)
  MBox keys <- readIORef (rtKeys rt)
  MBox rects <- readIORef (rtRects rt)
  cap' <- readIORef (rtCap rt)
  let !(I# cap#) = cap'
      max# = cap# -# 1#
      !(I# k#) = k
      start# = andI# k# max#
      writeFloats b# s0 =
        case writeFloatArray# rects b# x s0 of
          s1 -> case writeFloatArray# rects (b# +# 1#) y s1 of
            s2 -> case writeFloatArray# rects (b# +# 2#) w s2 of
              s3 -> writeFloatArray# rects (b# +# 3#) h s3
      go !i# s = case readIntArray# keys i# s of
        (# s', key# #)
          | isTrue# (key# ==# 0#) -> case writeFloats (i# *# 4#) s' of
              s2 -> case writeIntArray# keys i# k# s2 of
                s3 -> (# s3, True #)
          | isTrue# (key# ==# k#) -> case writeFloats (i# *# 4#) s' of
              s2 -> (# s2, False #)
          | otherwise -> go (if isTrue# (i# ==# max#) then 0# else i# +# 1#) s'
  fresh <- IO (go start#)
  when fresh (writeURef (rtCount rt) (count + 1))

-- | The rect recorded under a slot this frame, if any.
lookupRect :: RectTable -> Int -> IO (Maybe Rect)
lookupRect rt k = do
  MBox keys <- readIORef (rtKeys rt)
  MBox rects <- readIORef (rtRects rt)
  cap <- readIORef (rtCap rt)
  let !(I# k#) = k
      !(I# max#) = cap - 1
      !(I# start#) = k .&. (cap - 1)
      readFloats :: Int# -> State# RealWorld -> (# State# RealWorld, Float#, Float#, Float#, Float# #)
      readFloats base# s =
        case readFloatArray# rects base# s of
          (# s1, x #) -> case readFloatArray# rects (base# +# 1#) s1 of
            (# s2, y #) -> case readFloatArray# rects (base# +# 2#) s2 of
              (# s3, w #) -> case readFloatArray# rects (base# +# 3#) s3 of
                (# s4, h #) -> (# s4, x, y, w, h #)
      go :: Int# -> IO (Maybe Rect)
      go i# = IO $ \s -> case readIntArray# keys i# s of
        (# s', key# #)
          | isTrue# (key# ==# 0#) -> (# s', Nothing #)
          | isTrue# (key# ==# k#) -> case readFloats (i# *# 4#) s' of
              (# s'', x, y, w, h #) -> (# s'', Just (Rect (F# x) (F# y) (F# w) (F# h)) #)
          | otherwise -> case go (next i#) of
              IO cont -> cont s'
      next i# = if isTrue# (i# ==# max#) then 0# else i# +# 1#
  go start#

-- | Whether a slot was recorded this frame.
memberRect :: RectTable -> Int -> IO Bool
memberRect rt k = do
  MBox keys <- readIORef (rtKeys rt)
  cap <- readIORef (rtCap rt)
  let !(I# k#) = k
      !(I# max#) = cap - 1
      !(I# start#) = k .&. (cap - 1)
      go :: Int# -> IO Bool
      go i# = IO $ \s -> case readIntArray# keys i# s of
        (# s', key# #)
          | isTrue# (key# ==# 0#) -> (# s', False #)
          | isTrue# (key# ==# k#) -> (# s', True #)
          | otherwise -> case go (next i#) of
              IO cont -> cont s'
      next i# = if isTrue# (i# ==# max#) then 0# else i# +# 1#
  go start#

-- | Every recorded slot and rect, in slot order, for hosts and tests.
rectTableToList :: RectTable -> IO [(Int, Rect)]
rectTableToList rt = do
  MBox keys <- readIORef (rtKeys rt)
  MBox rects <- readIORef (rtRects rt)
  cap <- readIORef (rtCap rt)
  let
      collect :: Int -> [(Int, Rect)] -> IO [(Int, Rect)]
      collect i acc
        | i < 0 = pure acc
        | otherwise = do
            let !(I# i#) = i
            key <-
              IO $ \s -> case readIntArray# keys i# s of
                (# s', key# #) -> (# s', I# key# #)
            if key == 0
              then collect (i - 1) acc
              else do
                let !(I# base#) = i * 4
                r <-
                  IO $ \s -> case readFloatArray# rects base# s of
                    (# s1, x #) -> case readFloatArray# rects (base# +# 1#) s1 of
                      (# s2, y #) -> case readFloatArray# rects (base# +# 2#) s2 of
                        (# s3, w #) -> case readFloatArray# rects (base# +# 3#) s3 of
                          (# s4, h #) -> (# s4, Rect (F# x) (F# y) (F# w) (F# h) #)
                collect (i - 1) ((key, r) : acc)
  collect (cap - 1) []

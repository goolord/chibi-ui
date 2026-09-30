{-# LANGUAGE MagicHash #-}
{-# LANGUAGE UnboxedTuples #-}

-- | One unboxed mutable 'Int' cell. Per-quad and per-entry counters live in
-- these because an @IORef Int@ writes a freshly boxed 'Int' on every
-- update, which at a quad per glyph is real garbage; these write in place.
module ChibiUI.Internal.URef
  ( URef
  , newURef
  , readURef
  , writeURef
  ) where

import GHC.Exts (Int (..), MutableByteArray#, RealWorld, newByteArray#, readIntArray#, writeIntArray#)
import GHC.IO (IO (IO))

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

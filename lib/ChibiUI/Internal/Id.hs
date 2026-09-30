-- | Widget ids and the id context they are derived from, from nano-ui.
-- Ids count up in call order among siblings, and a container starts a new
-- count for its children; widget state is stored under the id, so the same
-- widgets must run in the same order every frame.
module ChibiUI.Internal.Id
  ( WidgetId (..)
  , IdContext (..)
  , initialIdPath
  , idContextWidgetId
  , hashWidgetId
  , mix64
  , mixFnv
  , scopeTag
  , keyedTag
  , enterScope
  , enterKeyed
  ) where

import Data.Bits (shiftR, xor)
import Data.Word (Word64)

-- | Stable store and interaction identity. Zero is reserved for no widget.
newtype WidgetId = WidgetId Word64
  deriving stock (Eq, Ord, Show)

-- | Parent-path hash and the next sibling's position within that path.
data IdContext = IdContext
  { currentId :: {-# UNPACK #-} !Word64
  , siblingId :: {-# UNPACK #-} !Word64
  }
  deriving stock (Eq, Show)

-- | The root scope's path hash, used at the start of each view pass.
initialIdPath :: Word64
initialIdPath = 0x243F6A8885A308D3

-- | Id of the next sibling in this context. A zero hash becomes 1, so
-- @WidgetId 0@ never names a real widget.
{-# INLINE idContextWidgetId #-}
idContextWidgetId :: IdContext -> WidgetId
idContextWidgetId (IdContext cid sid) =
  let
    raw = mix64 cid sid
   in
    if raw == 0 then WidgetId 1 else WidgetId raw

-- | Hash salt distinguishing an ordinary child scope from a keyed scope.
scopeTag :: Word64
scopeTag = 0x9E3779B185EBCA87

keyedTag :: Word64
keyedTag = 0xC2B2AE3D27D4EB4F

-- | Return the advanced parent and a fresh child context derived from its
-- sibling position and the supplied tag.
{-# INLINE enterScope #-}
enterScope :: Word64 -> IdContext -> (IdContext, IdContext)
enterScope tag parent = enterChild (siblingId parent) tag parent

-- | Return the advanced parent and a child path derived from the key, not the
-- sibling position. Keys must be unique within the parent.
{-# INLINE enterKeyed #-}
enterKeyed :: Word64 -> IdContext -> (IdContext, IdContext)
enterKeyed tag = enterChild tag keyedTag

-- | Advance the parent's sibling counter and derive a child path from the
-- parent path, @seed@ and @tag@.
{-# INLINE enterChild #-}
enterChild :: Word64 -> Word64 -> IdContext -> (IdContext, IdContext)
enterChild seed tag (IdContext pid sib) = (IdContext pid (sib + 1), IdContext (mix64 (mix64 pid seed) tag) 0)

-- | Unwrap the id's hash.
{-# INLINE hashWidgetId #-}
hashWidgetId :: WidgetId -> Word64
hashWidgetId (WidgetId w) = w

-- | Mix two 64-bit identifier components with wrapping arithmetic.
-- This is a non-cryptographic hash combiner.
{-# INLINE mix64 #-}
mix64 :: Word64 -> Word64 -> Word64
mix64 x y =
  let
    z = x + (y * 0x9E3779B97F4A7C15)
    z1 = z `xor` (z `shiftR` 30)
    z2 = z1 * 0xBF58476D1CE4E5B9
    z3 = z2 `xor` (z2 `shiftR` 27)
   in
    z3 * 0x94D049BB133111EB

-- | Combine two hash words with an FNV xor-and-multiply step.
{-# INLINE mixFnv #-}
mixFnv :: Word64 -> Word64 -> Word64
mixFnv x y = (x `xor` y) * 1099511628211

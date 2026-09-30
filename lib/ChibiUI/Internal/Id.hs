-- | Widget ids and the scope paths they are derived from, from nano-ui.
-- Ids count up in call order among siblings, and a container starts a new
-- count for its children; widget state is stored under the id, so the same
-- widgets must run in the same order every frame.
module ChibiUI.Internal.Id
  ( WidgetId (..)
  , initialIdPath
  , widgetIdAt
  , positionalPath
  , keyedPath
  , hashWidgetId
  ) where

import Data.Bits (shiftR, xor)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Word (Word64)

-- | Stable store and interaction identity. Zero is reserved for no widget.
newtype WidgetId = WidgetId Word64
  deriving stock (Eq, Ord, Show)

-- | The root scope's path hash, used at the start of each view pass.
initialIdPath :: Word64
initialIdPath = 0x243F6A8885A308D3

-- | The id of the widget at sibling position @sib@ in the scope at @path@.
-- A zero hash becomes 1, so @WidgetId 0@ never names a real widget.
{-# INLINE widgetIdAt #-}
widgetIdAt :: Word64 -> Word64 -> WidgetId
widgetIdAt path sib =
  let raw = mix64 path sib
   in if raw == 0 then WidgetId 1 else WidgetId raw

-- | The path of an ordinary child scope, entered at sibling position @sib@
-- of the scope at @path@.
positionalPath :: Word64 -> Word64 -> Word64
positionalPath path sib = mix64 (mix64 path sib) 0x9E3779B185EBCA87

-- | The path of a child scope derived from a key instead of its sibling
-- position, so it keeps its identity when siblings reorder. Keys must be
-- unique within the parent.
keyedPath :: Text -> Word64 -> Word64 -> Word64
keyedPath key path _ = mix64 (mix64 path (fnv1a key)) 0xC2B2AE3D27D4EB4F

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

-- | FNV-1a over a key's code points.
fnv1a :: Text -> Word64
fnv1a = T.foldl' (\acc c -> (acc `xor` fromIntegral (fromEnum c)) * 1099511628211) 0xcbf29ce484222325

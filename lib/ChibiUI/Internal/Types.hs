-- | Geometry in logical pixels and packed RGBA colours, from nano-ui.
-- Window coordinates start at the top-left, with x rightward and y downward.
module ChibiUI.Internal.Types
  ( V2 (..)
  , Rect (..)
  , Size (..)
  , Color (..)
  , colorRGBA
  , colorWhite
  , colorTransparent
  , colorR
  , colorG
  , colorB
  , colorA
  , colorFloats
  , clamp
  , clamp01
  , roundHalfUp
  , validScale
  , rectContains
  , rectNonEmpty
  , rectHit
  , rectUnion
  , rectIntersect
  , rectsOverlap
  , rectInflate
  , rectArea
  , v2Sub
  , foldUpTo
  ) where

import Data.Maybe (isJust)
import Data.Bits (shiftL, shiftR, (.&.), (.|.))
import Data.Word (Word8, Word32)

-- | A two-component point, offset, or vector; units depend on its use.
data V2 = V2
  { v2X :: {-# UNPACK #-} !Float
  , v2Y :: {-# UNPACK #-} !Float
  }
  deriving (Eq, Show)

-- | Width and height in logical pixels.
data Size = Size
  { sizeW :: {-# UNPACK #-} !Float
  , sizeH :: {-# UNPACK #-} !Float
  }
  deriving (Eq, Show)

-- | Top-left origin, width, and height in logical pixels. Hit tests include
-- the left and top edges and exclude the right and bottom edges.
data Rect = Rect
  { rectX :: {-# UNPACK #-} !Float
  , rectY :: {-# UNPACK #-} !Float
  , rectW :: {-# UNPACK #-} !Float
  , rectH :: {-# UNPACK #-} !Float
  }
  deriving (Eq, Show)

-- | Straight-alpha colour packed as @0xRRGGBBAA@, with 8 bits per channel.
newtype Color = Color Word32
  deriving (Eq, Show, Num)

-- | Pack red, green, blue, and alpha channels; alpha 0 is transparent, 255 opaque.
{-# INLINE colorRGBA #-}
colorRGBA :: Word8 -> Word8 -> Word8 -> Word8 -> Color
colorRGBA r g b a =
  Color $
    (fromIntegral r `shiftL` 24)
      .|. (fromIntegral g `shiftL` 16)
      .|. (fromIntegral b `shiftL` 8)
      .|. fromIntegral a

-- | Opaque white and fully transparent black.
colorWhite, colorTransparent :: Color
colorWhite = Color 0xFFFFFFFF
colorTransparent = Color 0

-- | Red channel, in the range 0-255.
{-# INLINE colorR #-}
colorR :: Color -> Word8
colorR (Color w) = fromIntegral ((w `shiftR` 24) .&. 0xFF)

-- | Green channel, in the range 0-255.
{-# INLINE colorG #-}
colorG :: Color -> Word8
colorG (Color w) = fromIntegral ((w `shiftR` 16) .&. 0xFF)

-- | Blue channel, in the range 0-255.
{-# INLINE colorB #-}
colorB :: Color -> Word8
colorB (Color w) = fromIntegral ((w `shiftR` 8) .&. 0xFF)

-- | Alpha channel, from 0 (transparent) to 255 (opaque).
{-# INLINE colorA #-}
colorA :: Color -> Word8
colorA (Color w) = fromIntegral (w .&. 0xFF)

-- | Red, green, blue and alpha, each normalised to 0-1.
{-# INLINE colorFloats #-}
colorFloats :: Color -> (Float, Float, Float, Float)
colorFloats c = (f (colorR c), f (colorG c), f (colorB c), f (colorA c))
  where
    f ch = fromIntegral ch / 255

-- | Restrict a value to inclusive lower and upper bounds, which must be ordered.
{-# INLINE clamp #-}
clamp :: Ord a => a -> a -> a -> a
clamp lo hi x = max lo (min hi x)

-- | Restrict a value to the inclusive range 0-1.
{-# INLINE clamp01 #-}
clamp01 :: Float -> Float
clamp01 x = clamp 0 1 x

-- | Round to the nearest integer, ties up: the device-pixel rounding shared
-- by layout and glyph pens. Not ties-to-even (@round@): at a fractional
-- scale, ties-to-even makes a column of same-sized rows land alternately
-- on and off half pixels, leaving uneven gaps.
{-# INLINE roundHalfUp #-}
roundHalfUp :: Float -> Int
roundHalfUp r =
  let f = floor r
   in if r - fromIntegral f >= 0.5 then f + 1 else f

-- | Whether a UI scale is usable: finite and positive.
validScale :: Float -> Bool
validScale s = s > 0 && not (isNaN s || isInfinite s)

-- | Test a point against half-open rectangle bounds.
{-# INLINE rectContains #-}
rectContains :: Rect -> V2 -> Bool
rectContains (Rect x y w h) (V2 px py) =
  px >= x && px < x + w && py >= y && py < y + h

-- | Whether both width and height are strictly positive.
{-# INLINE rectNonEmpty #-}
rectNonEmpty :: Rect -> Bool
rectNonEmpty r = rectW r > 0 && rectH r > 0

-- | Hit test that rejects empty and negative-size rectangles.
{-# INLINE rectHit #-}
rectHit :: Rect -> V2 -> Bool
rectHit r p = rectNonEmpty r && rectContains r p

-- | Smallest bounding rectangle containing both inputs. Empty inputs are
-- still included by their coordinates; filter them first if they mean no area.
{-# INLINE rectUnion #-}
rectUnion :: Rect -> Rect -> Rect
rectUnion (Rect x1 y1 w1 h1) (Rect x2 y2 w2 h2) =
  let x = min x1 x2
      y = min y1 y2
      xEnd = max (x1 + w1) (x2 + w2)
      yEnd = max (y1 + h1) (y2 + h2)
   in Rect x y (xEnd - x) (yEnd - y)

-- | Shared positive-area rectangle, or 'Nothing' for disjoint or touching edges.
{-# INLINE rectIntersect #-}
rectIntersect :: Rect -> Rect -> Maybe Rect
rectIntersect (Rect x1 y1 w1 h1) (Rect x2 y2 w2 h2) =
  let x = max x1 x2
      y = max y1 y2
      xEnd = min (x1 + w1) (x2 + w2)
      yEnd = min (y1 + h1) (y2 + h2)
      w = xEnd - x
      h = yEnd - y
   in if w > 0 && h > 0 then Just (Rect x y w h) else Nothing

-- | Whether two rectangles share positive area.
{-# INLINE rectsOverlap #-}
rectsOverlap :: Rect -> Rect -> Bool
rectsOverlap a b = isJust (rectIntersect a b)

-- | Extend every edge by the margin. A negative margin shrinks the rectangle.
{-# INLINE rectInflate #-}
rectInflate :: Float -> Rect -> Rect
rectInflate pad (Rect x y w h) =
  Rect (x - pad) (y - pad) (w + pad * 2) (h + pad * 2)

-- | Width times height. Requires non-negative dimensions for a geometric area.
{-# INLINE rectArea #-}
rectArea :: Rect -> Float
rectArea (Rect _ _ w h) = w * h

-- | Subtract corresponding components, for example the offset between points.
{-# INLINE v2Sub #-}
v2Sub :: V2 -> V2 -> V2
v2Sub (V2 x1 y1) (V2 x2 y2) = V2 (x1 - x2) (y1 - y2)

-- | Strict left fold over @0 .. n - 1@.
{-# INLINE foldUpTo #-}
foldUpTo :: Int -> (a -> Int -> IO a) -> a -> IO a
foldUpTo n f = go 0
  where
    go !i !acc
      | i >= n = pure acc
      | otherwise = f acc i >>= go (i + 1)

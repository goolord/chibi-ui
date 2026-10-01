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
  , colorFloats
  , clamp
  , clamp01
  , clampSpan
  , roundHalfUp
  , isFinite
  , validScale
  , rectNonEmpty
  , rectHit
  , rectUnion
  , rectIntersect
  , rectsOverlap
  , rectContains
  , rectInflate
  , rectCutLeft
  , rectClampInto
  , rectBottomLeft
  , rectArea
  , v2Sub
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

-- | Red, green, blue and alpha, each normalised to 0-1.
{-# INLINE colorFloats #-}
colorFloats :: Color -> (Float, Float, Float, Float)
colorFloats (Color w) = (channel 24, channel 16, channel 8, channel 0)
  where
    channel s = fromIntegral ((w `shiftR` s) .&. 0xFF) / 255

-- | Restrict a value to inclusive lower and upper bounds, which must be ordered.
{-# INLINE clamp #-}
clamp :: Ord a => a -> a -> a -> a
clamp lo hi x = max lo (min hi x)

-- | Restrict a value to the inclusive range 0-1.
{-# INLINE clamp01 #-}
clamp01 :: Float -> Float
clamp01 x = clamp 0 1 x

-- | Keep an offset within what a @content@ extent can move through a
-- @view@ extent: 0 up to their difference, or 0 when the view is larger.
{-# INLINE clampSpan #-}
clampSpan :: Float -> Float -> Float -> Float
clampSpan content view = clamp 0 (max 0 (content - view))

-- | Round to the nearest integer, ties up: the device-pixel rounding shared
-- by layout and glyph pens. Not ties-to-even (@round@): at a fractional
-- scale, ties-to-even makes a column of same-sized rows land alternately
-- on and off half pixels, leaving uneven gaps.
{-# INLINE roundHalfUp #-}
roundHalfUp :: Float -> Int
roundHalfUp r =
  let f = floor r
   in if r - fromIntegral f >= 0.5 then f + 1 else f

-- | Neither NaN nor infinite.
{-# INLINE isFinite #-}
isFinite :: RealFloat a => a -> Bool
isFinite x = not (isNaN x || isInfinite x)

-- | Whether a UI scale is usable: finite and positive.
validScale :: Float -> Bool
validScale s = s > 0 && isFinite s

-- | Whether both width and height are strictly positive.
{-# INLINE rectNonEmpty #-}
rectNonEmpty :: Rect -> Bool
rectNonEmpty r = rectW r > 0 && rectH r > 0

-- | Test a point against half-open rectangle bounds, rejecting empty and
-- negative-size rectangles.
{-# INLINE rectHit #-}
rectHit :: Rect -> V2 -> Bool
rectHit r@(Rect x y w h) (V2 px py) =
  rectNonEmpty r && px >= x && px < x + w && py >= y && py < y + h

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

-- | Whether the second rectangle lies wholly inside the first.
{-# INLINE rectContains #-}
rectContains :: Rect -> Rect -> Bool
rectContains (Rect x y w h) (Rect x' y' w' h') =
  x' >= x && y' >= y && x' + w' <= x + w && y' + h' <= y + h

-- | Extend every edge by the margin. A negative margin shrinks the rectangle.
{-# INLINE rectInflate #-}
rectInflate :: Float -> Rect -> Rect
rectInflate pad (Rect x y w h) =
  Rect (x - pad) (y - pad) (w + pad * 2) (h + pad * 2)

-- | Drop @d@ from the left edge, keeping the right edge where it is.
{-# INLINE rectCutLeft #-}
rectCutLeft :: Float -> Rect -> Rect
rectCutLeft d r = r {rectX = rectX r + d, rectW = max 0 (rectW r - d)}

-- | Move a rectangle the least distance that puts it inside a @w@ x @h@
-- area at the origin; one larger than the area keeps its top-left there.
{-# INLINE rectClampInto #-}
rectClampInto :: Size -> Rect -> Rect
rectClampInto (Size w h) (Rect x y rw rh) = Rect (clampSpan w rw x) (clampSpan h rh y) rw rh

-- | The bottom-left corner, as a menu below a widget opens from.
{-# INLINE rectBottomLeft #-}
rectBottomLeft :: Rect -> V2
rectBottomLeft r = V2 (rectX r) (rectY r + rectH r)

-- | Width times height. Requires non-negative dimensions for a geometric area.
{-# INLINE rectArea #-}
rectArea :: Rect -> Float
rectArea (Rect _ _ w h) = w * h

-- | Subtract corresponding components, for example the offset between points.
{-# INLINE v2Sub #-}
v2Sub :: V2 -> V2 -> V2
v2Sub (V2 x1 y1) (V2 x2 y2) = V2 (x1 - x2) (y1 - y2)

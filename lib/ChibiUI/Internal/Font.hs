{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UnboxedTuples #-}

-- | Text: RFont rasterizes the embedded TrueType font (a subset of Inter)
-- into one coverage atlas, and text draws as glyph quads from it. One font
-- and one line height; measurement walks glyph advances, so text is
-- variable-width.
--
-- Rasterized glyphs are memoized per raster size, Latin-1 in a table read
-- directly and the rest in a strict 'IntMap', so steady-state frames
-- measure and draw text without FFI crossings or per-glyph heap
-- allocation; a scale change drops the cache with the old C font.
module ChibiUI.Internal.Font
  ( Font
  , lineHeight
  , embeddedFont
  , newFont
  , fontFree
  , fontSetScale
  , fontMeasure
  , fontDrawText
  , fontAtlasPixels
  , fontScale
  , atlasSize
  , AtlasChange (..)
  , fontTakeDirty
  ) where

import Control.Monad (when)
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Unsafe as BSU
import Data.FileEmbed (embedFileRelative)
import Data.Int (Int32)
import Data.IntMap.Strict (IntMap)
import qualified Data.IntMap.Strict as IM
import Data.IORef
import Data.Text (Text)
import qualified Data.Text.Unsafe as TU
import Data.Word (Word8, Word32)
import Foreign.Marshal.Alloc (allocaBytes)
import Foreign.Ptr (Ptr, castPtr, nullPtr, plusPtr)
import Foreign.Storable (peekByteOff)
import GHC.IOArray (IOArray, newIOArray, unsafeReadIOArray, unsafeWriteIOArray)
import ChibiUI.Internal.Draw (DrawArena, emitQuadUV, texAtlas)
import ChibiUI.Internal.Types (Color, roundHalfUp, validScale)

-- | The embedded TrueType font, from nano-ui's SDL backend: a subset of
-- Inter (SIL OFL).
embeddedFont :: ByteString
embeddedFont = $(embedFileRelative "data/inter.ttf")

-- | The line height, in logical pixels: the RFont raster size is this
-- times the UI scale, and every single-line widget sizes by it. 16
-- matches nano-ui's default text size.
lineHeight :: Float
lineHeight = 16

-- | Atlas dimensions, in texels. 512x512 of coverage holds thousands of
-- glyphs at UI sizes.
atlasWidth, atlasHeight :: Int
atlasWidth = 512
atlasHeight = 512

-- | One rasterized glyph, in device pixels at the current size, with its
-- atlas rect as normalized UVs. All fields are unboxed into the
-- constructor, so a cache hit allocates nothing.
data GlyphDev = GlyphDev
  { gdU0 :: {-# UNPACK #-} !Float
  , gdV0 :: {-# UNPACK #-} !Float
  , gdU1 :: {-# UNPACK #-} !Float
  , gdV1 :: {-# UNPACK #-} !Float
  , gdW :: {-# UNPACK #-} !Float
  , gdH :: {-# UNPACK #-} !Float
  , gdX1 :: {-# UNPACK #-} !Float
  , gdY1 :: {-# UNPACK #-} !Float
  , gdAdvance :: {-# UNPACK #-} !Float
  }

-- | What a scale change rebuilds: the C handle, the scale and the raster
-- size it gives, the metrics, and the device-pixel space advance. One
-- strict record, so text calls read it with a single 'readIORef'.
data FontState = FontState
  { fsHandle :: !(Ptr ())
  , fsScale :: {-# UNPACK #-} !Float
  , fsSize :: {-# UNPACK #-} !Int
  -- ^ The raster size: the line height in device pixels.
  , fsFHeight :: {-# UNPACK #-} !Float
  , fsDescent :: {-# UNPACK #-} !Float
  , fsSpaceAdv :: {-# UNPACK #-} !Float
  -- ^ The space glyph's advance at the raster size, in device pixels.
  , fsLow :: !(IOArray Int GlyphDev)
  -- ^ Rasterized glyphs below 'lowGlyphs' by code point, else 'missGlyph'.
  , fsHigh :: !(IORef (IntMap GlyphDev))
  -- ^ Rasterized glyphs from 'lowGlyphs' up, by code point.
  }

-- | The loaded font: its bytes for scale rebuilds, and the state at the
-- current scale.
data Font = Font
  { fBytes :: !BS.ByteString
  , fState :: !(IORef FontState)
  }

-- | Code points cached in the direct table: Latin-1.
lowGlyphs :: Int
lowGlyphs = 256

foreign import ccall unsafe "chibi_rfont_init"
  c_init :: Ptr Word8 -> Word32 -> Word32 -> Word32 -> Word32 -> IO (Ptr ())

foreign import ccall unsafe "chibi_rfont_free"
  c_free :: Ptr () -> IO ()

foreign import ccall unsafe "chibi_rfont_glyph"
  c_glyph :: Ptr () -> Word32 -> Word32 -> Ptr Word8 -> IO ()

foreign import ccall unsafe "chibi_rfont_metrics"
  c_metrics :: Ptr () -> Ptr Float -> Ptr Float -> Ptr Float -> IO ()

foreign import ccall unsafe "chibi_rfont_atlas_pixels"
  c_atlasPixels :: Ptr () -> IO (Ptr Word8)

foreign import ccall unsafe "chibi_rfont_take_dirty"
  c_takeDirty :: Ptr () -> Ptr Int32 -> Ptr Int32 -> IO Int32

-- | The chibi_rfont.h ChibiGlyph struct: four i32 atlas coords, then five
-- floats.
glyphBytes :: Int
glyphBytes = 36

peekGlyph :: Ptr Word8 -> IO GlyphDev
peekGlyph p = do
  -- The C struct's atlas coords are 4-byte int32s; read them as Int32 so
  -- the peek doesn't swallow the neighbouring field into an 8-byte Int.
  let i off = fromIntegral <$> (peekByteOff p off :: IO Int32)
      fl off = peekByteOff p off :: IO Float
  ax <- i 0
  ay <- i 4
  ax2 <- i 8
  ay2 <- i 12
  w <- fl 16
  h <- fl 20
  x1 <- fl 24
  y1 <- fl 28
  adv <- fl 32
  let u x = fromIntegral (x :: Int) / fromIntegral atlasWidth
      v y = fromIntegral (y :: Int) / fromIntegral atlasHeight
  pure GlyphDev {gdU0 = u ax, gdV0 = v ay, gdU1 = u ax2, gdV1 = v ay2, gdW = w, gdH = h, gdX1 = x1, gdY1 = y1, gdAdvance = adv}

-- | Load the font at scale 1. The bytes are copied into C memory, so the
-- caller may release them.
newFont :: ByteString -> IO Font
newFont bytes = Font bytes <$> (newIORef =<< loadState bytes 1)

-- | (Re)load the C font at a UI scale. The raster size is the line height
-- in device pixels; RFont caches glyphs per size, so a scale change starts
-- a fresh font and atlas, and the Haskell-side glyph caches with them.
fontSetScale :: Font -> Float -> IO ()
fontSetScale f scale = do
  let scale' = if validScale scale then scale else 1
  old <- readIORef (fState f)
  when (scale' /= fsScale old) $ do
    when (fsHandle old /= nullPtr) (c_free (fsHandle old))
    writeIORef (fState f) =<< loadState (fBytes f) scale'

-- | A fresh C font and empty glyph caches at a valid UI scale.
loadState :: ByteString -> Float -> IO FontState
loadState bytes scale = do
  let sizeD = max 8 (round (lineHeight * scale) :: Int)
  h <-
    BSU.unsafeUseAsCStringLen bytes $ \(p, len) ->
      c_init (castPtr p) (fromIntegral len) (fromIntegral sizeD) (fromIntegral atlasWidth) (fromIntegral atlasHeight)
  when (h == nullPtr) $ fail "chibi-ui: font failed to load"
  low <- newIOArray (0, lowGlyphs - 1) missGlyph
  high <- newIORef IM.empty
  (fh, ds, sa) <- allocaBytes 12 $ \m -> do
    c_metrics h m (plusPtr m 4) (plusPtr m 8)
    (,,) <$> (peekByteOff m 0 :: IO Float) <*> (peekByteOff m 4 :: IO Float) <*> (peekByteOff m 8 :: IO Float)
  pure
    FontState
      { fsHandle = h
      , fsScale = scale
      , fsSize = sizeD
      , fsFHeight = fh
      , fsDescent = ds
        -- The space advance in device pixels, precomputed: a space has
        -- no glyph box, so its width comes from the font's hmtx entry
        -- scaled by the raster size.
      , fsSpaceAdv = if fh > 0 then sa * fromIntegral sizeD / fh else 0
      , fsLow = low
      , fsHigh = high
      }

-- | Device pixels per logical pixel the font rasterizes at.
fontScale :: Font -> IO Float
fontScale f = fsScale <$> readIORef (fState f)

-- | Free the C font. The record is dead afterwards.
fontFree :: Font -> IO ()
fontFree f = do
  st <- readIORef (fState f)
  when (fsHandle st /= nullPtr) (c_free (fsHandle st))
  writeIORef (fState f) st {fsHandle = nullPtr}

-- | The width of one line of text, in logical pixels: the sum of glyph
-- advances. Newlines advance nothing. Measuring rasterizes the glyphs, so
-- repeated frames are cache reads.
fontMeasure :: Font -> Text -> IO Float
fontMeasure f t = do
  st <- readIORef (fState f)
  dev <- walkGlyphs st t 0 (\_ _ -> pure ())
  pure (dev * recip (fsScale st))

-- | Walk one line's glyphs from a device-pixel pen, visiting each glyph at
-- its pen, and return the final pen. Measuring and drawing both walk here,
-- so drawn spacing is exactly what 'fontMeasure' reports: newlines advance
-- nothing, tabs read as spaces. The walk goes by UTF-8 byte offsets and
-- inlines at each caller with its visitor, so a steady-state walk
-- allocates nothing per character.
{-# INLINE walkGlyphs #-}
walkGlyphs :: FontState -> Text -> Float -> (Float -> GlyphDev -> IO ()) -> IO Float
walkGlyphs st t pen0 visit = go pen0 0
  where
    end = TU.lengthWord8 t
    go !pen !i
      | i >= end = pure pen
      | otherwise = case charAt t i of
          (# cp, d #)
            | cp == cpLF || cp == cpCR -> go pen (i + d)
            | cp == cpSpace || cp == cpTab -> go (pen + fsSpaceAdv st) (i + d)
            | otherwise -> do
                g <- glyph st cp
                visit pen g
                go (pen + max 0 (gdAdvance g)) (i + d)

-- | The sentinel 'findWithDefault' returns for a glyph not yet
-- rasterized: a negative advance, which no real glyph has.
missGlyph :: GlyphDev
missGlyph = GlyphDev 0 0 0 0 0 0 0 0 (-1)

-- Code points the walker dispatches on, without constructing a 'Char'.
cpSpace, cpTab, cpLF, cpCR :: Int
cpSpace = 32
cpTab = 9
cpLF = 10
cpCR = 13

-- | Decode the character at byte offset @i@ as @(code point, byte length)@.
-- The unboxed pair keeps the measure and draw loops comparing code points
-- as ints, with no 'Char' boxing at the loop boundary; @iter@ itself is
-- CPR-optimized in text, so the decode allocates nothing either.
{-# INLINE charAt #-}
charAt :: Text -> Int -> (# Int, Int #)
charAt t !i = case TU.iter t i of
  TU.Iter c d -> (# fromEnum c, d #)

-- | The glyph for a code point, rasterized and cached on a miss. Without
-- a C font it stays 'missGlyph', which advances and draws nothing.
{-# INLINE glyph #-}
glyph :: FontState -> Int -> IO GlyphDev
glyph st cp = do
  g <-
    if cp < lowGlyphs
      then unsafeReadIOArray (fsLow st) cp
      else IM.findWithDefault missGlyph cp <$> readIORef (fsHigh st)
  if gdAdvance g >= 0 || fsHandle st == nullPtr then pure g else rasterize st cp

rasterize :: FontState -> Int -> IO GlyphDev
rasterize st cp = do
  g <- allocaBytes glyphBytes $ \p -> do
    c_glyph (fsHandle st) (fromIntegral cp) (fromIntegral (fsSize st)) p
    peekGlyph p
  if cp < lowGlyphs
    then unsafeWriteIOArray (fsLow st) cp g
    else modifyIORef' (fsHigh st) (IM.insert cp g)
  pure g

-- | Draw one line of text as glyph quads into the draw arena, with the
-- line box's top-left at the logical pen. Baseline math follows RFont's:
-- the baseline sits @(fheight + descent) / fheight@ of the size below the
-- line top, and each glyph hangs from the baseline by its bearings.
--
-- Glyphs are rasterized once at whole device pixels, so their quads must
-- land back on whole device pixels: a fractional pen makes nearest-texel
-- sampling drop and duplicate coverage columns (blocky, uneven strokes).
-- The baseline is snapped per line and each glyph's pen per glyph, in
-- device space, the way terminals place glyphs; advances still accumulate
-- fractionally, so spacing stays true to 'fontMeasure'.
fontDrawText :: Font -> DrawArena -> Float -> Float -> Color -> Text -> IO ()
fontDrawText f arena penX penY col t = do
  st@FontState {fsHandle = h, fsScale = scale, fsSize = sizeD, fsFHeight = fh, fsDescent = ds} <-
    readIORef (fState f)
  when (h /= nullPtr) $ do
    let baseline =
          if fh > 0
            then fromIntegral sizeD * (fh + ds) / fh
            else fromIntegral sizeD
        baseY = fromIntegral (roundHalfUp (penY * scale + baseline))
        invScale = recip scale
    _ <- walkGlyphs st t (penX * scale) $ \pen g ->
      when (gdW g > 0 && gdH g > 0) $
        let px = fromIntegral (roundHalfUp pen)
         in emitQuadUV arena texAtlas
              ((px + gdX1 g) * invScale)
              ((baseY + gdY1 g) * invScale)
              ((px + gdX1 g + gdW g) * invScale)
              ((baseY + gdY1 g + gdH g) * invScale)
              col
              (gdU0 g)
              (gdV0 g)
              (gdU1 g)
              (gdV1 g)
    pure ()

-- | The atlas coverage bytes. The pointer is stable for the font's
-- lifetime; read it only while the dirty flag says new glyphs exist.
fontAtlasPixels :: Font -> IO (Ptr Word8)
fontAtlasPixels f = do
  h <- fsHandle <$> readIORef (fState f)
  if h /= nullPtr then c_atlasPixels h else pure nullPtr

-- | The atlas dimensions, in texels, the same for every font.
atlasSize :: (Int, Int)
atlasSize = (atlasWidth, atlasHeight)

-- | What changed in the atlas since the last call.
data AtlasChange
  = AtlasClean
  | AtlasRows !Int !Int
  -- ^ New glyphs landed in rows @[y0, y1)@; every texel outside them is
    -- as last taken, so existing glyphs still sample what they did.
  | AtlasFresh
  -- ^ A new atlas, after the font loaded or rebuilt at another scale:
    -- upload all of it.

-- | Take what changed in the atlas since the last call, so the backend
-- uploads just that.
fontTakeDirty :: Font -> IO AtlasChange
fontTakeDirty f = do
  h <- fsHandle <$> readIORef (fState f)
  if h == nullPtr
    then pure AtlasClean
    else allocaBytes 8 $ \p -> do
      change <- c_takeDirty h p (p `plusPtr` 4)
      y0 <- peekByteOff p 0 :: IO Int32
      y1 <- peekByteOff p 4 :: IO Int32
      pure $ case change of
        2 -> AtlasFresh
        1 -> AtlasRows (fromIntegral y0) (fromIntegral y1)
        _ -> AtlasClean

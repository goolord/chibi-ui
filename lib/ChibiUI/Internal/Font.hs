{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UnboxedTuples #-}

-- | Text: RFont rasterizes the embedded TrueType font (a subset of Inter)
-- into one coverage atlas, and text draws as glyph quads from it. One font
-- and one line height; measurement walks glyph advances, so text is
-- variable-width.
--
-- Rasterized glyphs are memoized per raster size in a strict 'IntMap', so
-- steady-state frames measure and draw text without FFI crossings or
-- per-glyph heap allocation; a scale change drops the cache with the old
-- C font.
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
  , fontAtlasSize
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
import ChibiUI.Internal.Draw (DrawArena, emitQuadUV)
import ChibiUI.Internal.Types (Color)

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

-- | One rasterized glyph, in device pixels at the current size. All fields
-- are unboxed into the constructor, so a cache hit allocates nothing.
data GlyphDev = GlyphDev
  { gdAX :: {-# UNPACK #-} !Int
  , gdAY :: {-# UNPACK #-} !Int
  , gdAX2 :: {-# UNPACK #-} !Int
  , gdAY2 :: {-# UNPACK #-} !Int
  , gdW :: {-# UNPACK #-} !Float
  , gdH :: {-# UNPACK #-} !Float
  , gdX1 :: {-# UNPACK #-} !Float
  , gdY1 :: {-# UNPACK #-} !Float
  , gdAdvance :: {-# UNPACK #-} !Float
  }

-- | The loaded font: the C handle, the font's bytes for scale rebuilds,
-- the metrics the raster size derives from, the device-pixel space
-- advance, and the glyph cache for the current raster size.
data Font = Font
  { fHandle :: !(IORef (Ptr ()))
  , fBytes :: !BS.ByteString
  , fScale :: !(IORef Float)
  , fFHeight :: !(IORef Float)
  , fDescent :: !(IORef Float)
  , fSpaceAdvDev :: !(IORef Float)
  -- ^ The space glyph's advance at the current raster size, in device
  -- pixels.
  , fGlyphs :: !(IORef (IntMap GlyphDev))
  -- ^ Rasterized glyphs by code point, valid for the current handle and
  -- raster size only.
  }

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
  c_takeDirty :: Ptr () -> IO Int32

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
  pure GlyphDev {gdAX = ax, gdAY = ay, gdAX2 = ax2, gdAY2 = ay2, gdW = w, gdH = h, gdX1 = x1, gdY1 = y1, gdAdvance = adv}

-- | Load the font at scale 1. The bytes are copied into C memory, so the
-- caller may release them.
newFont :: ByteString -> IO Font
newFont bytes = do
  handle <- newIORef nullPtr
  scaleRef <- newIORef 0
  fh <- newIORef 0
  ds <- newIORef 0
  sa <- newIORef 0
  glyphs <- newIORef IM.empty
  let f =
        Font
          { fHandle = handle
          , fBytes = bytes
          , fScale = scaleRef
          , fFHeight = fh
          , fDescent = ds
          , fSpaceAdvDev = sa
          , fGlyphs = glyphs
          }
  fontSetScale f 1
  pure f

-- | (Re)load the C font at a UI scale. The raster size is the line height
-- in device pixels; RFont caches glyphs per size, so a scale change starts
-- a fresh font and atlas, and the Haskell-side glyph cache with them.
fontSetScale :: Font -> Float -> IO ()
fontSetScale f scale = do
  let scale' = if scale > 0 && not (isNaN scale || isInfinite scale) then scale else 1
  old <- readIORef (fScale f)
  if scale' == old
    then pure ()
    else do
      oldHandle <- readIORef (fHandle f)
      if oldHandle /= nullPtr
        then c_free oldHandle
        else pure ()
      let sizeD = max 8 (round (lineHeight * scale') :: Int)
      h <-
        BSU.unsafeUseAsCStringLen (fBytes f) $ \(p, len) ->
          c_init (castPtr p) (fromIntegral len) (fromIntegral sizeD) (fromIntegral atlasWidth) (fromIntegral atlasHeight)
      if h == nullPtr
        then fail "chibi-ui: font failed to load"
        else do
          writeIORef (fHandle f) h
          allocaBytes 12 $ \m -> do
            c_metrics h m (plusPtr m 4) (plusPtr m 8)
            fh <- peekByteOff m 0 :: IO Float
            ds <- peekByteOff m 4 :: IO Float
            sa <- peekByteOff m 8 :: IO Float
            writeIORef (fFHeight f) fh
            writeIORef (fDescent f) ds
            -- The space advance in device pixels, precomputed: a space
            -- has no glyph box, so its width comes from the font's hmtx
            -- entry scaled by the raster size.
            writeIORef (fSpaceAdvDev f) (if fh > 0 then sa * fromIntegral sizeD / fh else 0)
          writeIORef (fGlyphs f) IM.empty
          writeIORef (fScale f) scale'

-- | Free the C font. The record is dead afterwards.
fontFree :: Font -> IO ()
fontFree f = do
  h <- readIORef (fHandle f)
  if h /= nullPtr then c_free h else pure ()
  writeIORef (fHandle f) nullPtr

-- | The device-pixel raster size at the current scale.
currentSize :: Font -> IO Word32
currentSize f = do
  scale <- readScale f
  pure (fromIntegral (max 8 (round (lineHeight * scale) :: Int)))

-- | The space advance at the current raster size, in device pixels.
spaceAdvDev :: Font -> IO Float
spaceAdvDev f = readIORef (fSpaceAdvDev f)

-- | The width of one line of text, in logical pixels: the sum of glyph
-- advances. Newlines advance nothing. Measuring rasterizes the glyphs, so
-- repeated frames are cache reads; the loop walks the text by UTF-8 byte
-- offsets and the glyph cache by pure map lookups, allocating nothing per
-- character.
fontMeasure :: Font -> Text -> IO Float
fontMeasure f t = do
  scale <- readScale f
  glyphs <- readIORef (fGlyphs f)
  sa <- spaceAdvDev f
  let end = TU.lengthWord8 t
      go !ms !acc !i
        | i >= end = pure acc
        | otherwise = case charAt t i of
            (# cp, d #)
              | cp == cpSpace || cp == cpTab -> go ms (acc + sa) (i + d)
              | cp == cpLF || cp == cpCR -> go ms acc (i + d)
              | otherwise -> case IM.findWithDefault missGlyph cp ms of
                  g
                    | gdAdvance g >= 0 -> go ms (acc + gdAdvance g) (i + d)
                    | otherwise -> do
                        (ms', g') <- rasterizeInto ms f cp
                        go ms' (acc + max 0 (gdAdvance g')) (i + d)
  dev <- go glyphs 0 0
  pure (dev * recip scale)

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

-- | Rasterize a code point, add it to the font's cache, and return the
-- glyph with the updated map.
rasterizeInto :: IntMap GlyphDev -> Font -> Int -> IO (IntMap GlyphDev, GlyphDev)
rasterizeInto glyphs f cp = do
  h <- readIORef (fHandle f)
  g <-
    if h == nullPtr
      then pure missGlyph
      else do
        sizeD <- currentSize f
        allocaBytes glyphBytes $ \p -> do
          c_glyph h (fromIntegral cp) sizeD p
          peekGlyph p
  g `seq` writeIORef (fGlyphs f) (IM.insert cp g glyphs)
  pure (IM.insert cp g glyphs, g)

-- | Draw one line of text as glyph quads into the draw arena, with the
-- line box's top-left at the logical pen. Baseline math follows RFont's:
-- the baseline sits @(fheight + descent) / fheight@ of the size below the
-- line top, and each glyph hangs from the baseline by its bearings. Tabs
-- read as spaces; newlines advance nothing. The walk and the cached-glyph
-- lookups are unboxed, so a steady-state draw allocates nothing per
-- character.
--
-- Glyphs are rasterized once at whole device pixels, so their quads must
-- land back on whole device pixels: a fractional pen makes nearest-texel
-- sampling drop and duplicate coverage columns (blocky, uneven strokes).
-- The baseline is snapped per line and each glyph's pen per glyph, in
-- device space, the way terminals place glyphs; advances still accumulate
-- fractionally, so spacing stays true to 'fontMeasure'.
fontDrawText :: Font -> DrawArena -> Float -> Float -> Color -> Text -> IO ()
fontDrawText f arena penX penY col t = do
  scale <- readScale f
  h <- readIORef (fHandle f)
  when (h /= nullPtr) $ do
    sizeD <- currentSize f
    fh <- readIORef (fFHeight f)
    ds <- readIORef (fDescent f)
    sa <- spaceAdvDev f
    glyphs <- readIORef (fGlyphs f)
    let baseline =
          if fh > 0
            then fromIntegral sizeD * (fh + ds) / fh
            else fromIntegral sizeD
        baseY = fromIntegral (round (penY * scale + baseline) :: Int)
        invScale = recip scale
        atlasW = fromIntegral atlasWidth :: Float
        atlasH = fromIntegral atlasHeight :: Float
        end = TU.lengthWord8 t
        go !ms !pen !i
          | i >= end = pure ()
          | otherwise = case charAt t i of
              (# cp, d #)
                | cp == cpLF || cp == cpCR -> go ms pen (i + d)
                | cp == cpSpace || cp == cpTab -> go ms (pen + sa) (i + d)
                | otherwise -> case IM.findWithDefault missGlyph cp ms of
                    g ->
                      if gdAdvance g < 0
                        then do
                          (ms', g') <- rasterizeInto ms f cp
                          if gdAdvance g' < 0
                            then go ms' pen (i + d) -- no font; skip it
                            else go ms' pen i -- retry as a hit, emitting it
                        else do
                          when (gdW g > 0 && gdH g > 0) $
                            let px = fromIntegral (round pen :: Int)
                            in emitQuadUV
                              arena
                              ((px + gdX1 g) * invScale)
                              ((baseY + gdY1 g) * invScale)
                              ((px + gdX1 g + gdW g) * invScale)
                              ((baseY + gdY1 g + gdH g) * invScale)
                              col
                              (fromIntegral (gdAX g) / atlasW)
                              (fromIntegral (gdAY g) / atlasH)
                              (fromIntegral (gdAX2 g) / atlasW)
                              (fromIntegral (gdAY2 g) / atlasH)
                          go ms (pen + gdAdvance g) (i + d)
    go glyphs (penX * scale) 0

readScale :: Font -> IO Float
readScale f = do
  scale <- readIORef (fScale f)
  pure (if scale > 0 then scale else 1)

-- | The atlas coverage bytes. The pointer is stable for the font's
-- lifetime; read it only while the dirty flag says new glyphs exist.
fontAtlasPixels :: Font -> IO (Ptr Word8)
fontAtlasPixels f = do
  h <- readIORef (fHandle f)
  if h /= nullPtr then c_atlasPixels h else pure nullPtr

-- | The atlas dimensions, in texels.
fontAtlasSize :: Font -> IO (Int, Int)
fontAtlasSize _ = pure (atlasWidth, atlasHeight)

-- | Whether any glyph has been rasterized since the last call, so the
-- backend should re-upload the atlas.
fontTakeDirty :: Font -> IO Bool
fontTakeDirty f = do
  h <- readIORef (fHandle f)
  if h /= nullPtr
    then (/= 0) <$> c_takeDirty h
    else pure False

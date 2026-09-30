{-# LANGUAGE TemplateHaskell #-}

-- | Text: RFont rasterizes the embedded TrueType font (a subset of Inter)
-- into one coverage atlas, and text draws as glyph quads from it. One font
-- and one line height; measurement walks glyph advances, so text is
-- variable-width.
module ChibiUI.Internal.Font
  ( Font
  , GlyphQuad (..)
  , lineHeight
  , embeddedFont
  , newFont
  , fontFree
  , fontSetScale
  , fontMeasure
  , fontGlyphs
  , fontAtlasPixels
  , fontAtlasSize
  , fontTakeDirty
  ) where

import Data.ByteString (ByteString)
import qualified Data.ByteString.Unsafe as BSU
import Data.FileEmbed (embedFileRelative)
import Data.IORef
import Data.Int (Int32)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Word (Word8, Word32)
import Foreign.Marshal.Alloc (allocaBytes)
import Foreign.Ptr (Ptr, castPtr, nullPtr, plusPtr)
import Foreign.Storable (peekByteOff)

-- | The embedded TrueType font, from nano-ui's SDL backend: a subset of
-- Inter (SIL OFL).
embeddedFont :: ByteString
embeddedFont = $(embedFileRelative "data/inter.ttf")

-- | The line height, in logical pixels: the RFont raster size is this
-- times the UI scale, and every single-line widget sizes by it.
lineHeight :: Float
lineHeight = 13

-- | Atlas dimensions, in texels. 512x512 of coverage holds thousands of
-- glyphs at UI sizes.
atlasWidth, atlasHeight :: Int
atlasWidth = 512
atlasHeight = 512

-- | One rasterized glyph, in device pixels at the current size.
data GlyphDev = GlyphDev
  { gdAX :: !Int
  , gdAY :: !Int
  , gdAX2 :: !Int
  , gdAY2 :: !Int
  , gdW :: !Float
  , gdH :: !Float
  , gdX1 :: !Float
  , gdY1 :: !Float
  , gdAdvance :: !Float
  }

-- | One glyph quad: corners @x0, y0, x1, y1@ in logical pixels and UV
-- corners @u0, v0, u1, v1@ into the font atlas.
data GlyphQuad = GlyphQuad
  { gqX0 :: !Float
  , gqY0 :: !Float
  , gqX1 :: !Float
  , gqY1 :: !Float
  , gqU0 :: !Float
  , gqV0 :: !Float
  , gqU1 :: !Float
  , gqV1 :: !Float
  }

-- | The loaded font: the C handle, the font's bytes for scale rebuilds,
-- and the metrics the raster size derives from.
data Font = Font
  { fHandle :: !(IORef (Ptr ()))
  , fBytes :: !ByteString
  , fScale :: !(IORef Float)
  , fFHeight :: !(IORef Float)
  , fDescent :: !(IORef Float)
  , fSpaceAdv :: !(IORef Float)
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
  let f =
        Font
          { fHandle = handle
          , fBytes = bytes
          , fScale = scaleRef
          , fFHeight = fh
          , fDescent = ds
          , fSpaceAdv = sa
          }
  fontSetScale f 1
  pure f

-- | (Re)load the C font at a UI scale. The raster size is the line height
-- in device pixels; RFont caches glyphs per size, so a scale change starts
-- a fresh font and atlas.
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
            writeIORef (fSpaceAdv f) sa
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

-- | Rasterize (or fetch) one glyph, in device pixels. Tabs read as spaces;
-- a missing glyph comes back blank with no advance.
glyphDev :: Font -> Char -> IO GlyphDev
glyphDev f c = do
  h <- readIORef (fHandle f)
  if h == nullPtr
    then pure blank
    else do
      sizeD <- currentSize f
      allocaBytes glyphBytes $ \p -> do
        c_glyph h (fromIntegral (fromEnum c)) sizeD p
        peekGlyph p
  where
    blank = GlyphDev 0 0 0 0 0 0 0 0 0

-- | The space advance at the current raster size, in device pixels. A
-- space has no glyph box, so stb returns a blank glyph for it; the width
-- comes from the font's hmtx entry instead.
spaceAdvDev :: Font -> IO Float
spaceAdvDev f = do
  sa <- readIORef (fSpaceAdv f)
  fh <- readIORef (fFHeight f)
  sizeD <- currentSize f
  pure (if fh > 0 then sa * fromIntegral sizeD / fh else 0)

-- | The width of one line of text, in logical pixels: the sum of glyph
-- advances. Newlines advance nothing. Measuring rasterizes the glyphs, so
-- repeated frames are cache reads.
fontMeasure :: Font -> Text -> IO Float
fontMeasure f t = do
  scale <- readScale f
  dev <- T.foldl' step (pure 0) (T.map visibleChar t)
  pure (dev / scale)
  where
    visibleChar c = if c == '\t' then ' ' else c
    step accIO c = do
      acc <- accIO
      adv <-
        if c == ' '
          then spaceAdvDev f
          else gdAdvance <$> glyphDev f c
      pure (acc + adv)

-- | The quads of one line of text, with the line box's top-left at the
-- logical pen. Baseline math follows RFont's: the baseline sits
-- @(fheight + descent) / fheight@ of the size below the line top, and each
-- glyph hangs from the baseline by its bearings.
fontGlyphs :: Font -> Float -> Float -> Text -> IO [GlyphQuad]
fontGlyphs f penX penY t = do
  scale <- readScale f
  h <- readIORef (fHandle f)
  if h == nullPtr
    then pure []
    else do
      sizeD <- currentSize f
      fh <- readIORef (fFHeight f)
      ds <- readIORef (fDescent f)
      let baseline =
            if fh > 0
              then fromIntegral sizeD * (fh + ds) / fh
              else fromIntegral sizeD
          penXd = penX * scale
          baseY = penY * scale + baseline
          toLogical q =
            q
              { gqX0 = gqX0 q / scale
              , gqY0 = gqY0 q / scale
              , gqX1 = gqX1 q / scale
              , gqY1 = gqY1 q / scale
              }
          go _ rest | T.null rest = pure []
          go !penAcc rest = case T.uncons rest of
            Nothing -> pure []
            Just (c, cs)
              | c == '\n' || c == '\r' -> go penAcc cs
              | c == ' ' -> do
                  -- A space draws nothing and advances by the font's own
                  -- space width.
                  sa <- spaceAdvDev f
                  go (penAcc + sa) cs
              | otherwise -> do
                  g <- glyphDev f c
                  let quad
                        | gdW g > 0 && gdH g > 0 =
                            [ GlyphQuad
                              { gqX0 = penAcc + gdX1 g
                              , gqY0 = baseY + gdY1 g
                              , gqX1 = penAcc + gdX1 g + gdW g
                              , gqY1 = baseY + gdY1 g + gdH g
                              , gqU0 = fromIntegral (gdAX g) / fromIntegral atlasWidth
                              , gqV0 = fromIntegral (gdAY g) / fromIntegral atlasHeight
                              , gqU1 = fromIntegral (gdAX2 g) / fromIntegral atlasWidth
                              , gqV1 = fromIntegral (gdAY2 g) / fromIntegral atlasHeight
                              }
                            ]
                        | otherwise = []
                  rest' <- go (penAcc + gdAdvance g) cs
                  pure (quad ++ rest')
      quads <- go penXd (T.map (\c -> if c == '\t' then ' ' else c) t)
      pure (map toLogical quads)

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

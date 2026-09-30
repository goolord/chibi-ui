-- | The theme: the colours and spacing widgets draw with. Flat surfaces,
-- 1px borders and square corners, so geometry matches hit boxes.
module ChibiUI.Internal.Style
  ( Theme (..)
  , defaultTheme
  , lightTheme
  , widgetPad
  , fieldPad
  , fieldHeight
  , TextAlign (..)
  , alignedTextY
  ) where

import ChibiUI.Internal.Types (Color, Rect (..), colorRGBA)
import ChibiUI.Internal.Font (lineHeight)

data TextAlign = AlignTop | AlignMiddle | AlignBottom
  deriving (Eq, Show)

alignedTextY :: TextAlign -> Rect -> Float
alignedTextY align r = rectY r + max 0 (rectH r - lineHeight) * case align of
  AlignTop -> 0
  AlignMiddle -> 0.5
  AlignBottom -> 1

-- | Colours in logical-pixel space. @themeWindow@ is the page behind
-- everything; surfaces are buttons, fields, table headers and scrollbar
-- tracks; @themeAccent@ marks focus and selection.
data Theme = Theme
  { themeWindow :: !Color
  , themeSurface :: !Color
  , themeSurfaceHover :: !Color
  , themeSurfaceActive :: !Color
  , themeBorder :: !Color
  , themeText :: !Color
  , themeTextDim :: !Color
  , themeAccent :: !Color
  , themeAccentText :: !Color
  , themeRowAlt :: !Color
  , themeRowHover :: !Color
  , themeGap :: !Float
  -- ^ Space between widgets laid out one after another.
  , themeWindowPad :: !Float
  , themeTextAlign :: !TextAlign
  -- ^ Space between the window edge and the first widget.
  }
  deriving (Eq, Show)

-- | A dark theme.
defaultTheme :: Theme
defaultTheme =
  Theme
    { themeWindow = colorRGBA 24 24 27 255
    , themeSurface = colorRGBA 39 39 42 255
    , themeSurfaceHover = colorRGBA 52 52 56 255
    , themeSurfaceActive = colorRGBA 63 63 68 255
    , themeBorder = colorRGBA 82 82 88 255
    , themeText = colorRGBA 220 220 218 255
    , themeTextDim = colorRGBA 145 145 148 255
    , themeAccent = colorRGBA 98 160 234 255
    , themeAccentText = colorRGBA 18 18 20 255
    , themeRowAlt = colorRGBA 31 31 34 255
    , themeRowHover = colorRGBA 46 46 50 255
    , themeGap = 6
    , themeWindowPad = 10
    , themeTextAlign = AlignMiddle
    }

-- | A light theme.
lightTheme :: Theme
lightTheme =
  Theme
    { themeWindow = colorRGBA 247 247 245 255
    , themeSurface = colorRGBA 255 255 255 255
    , themeSurfaceHover = colorRGBA 236 236 233 255
    , themeSurfaceActive = colorRGBA 224 224 220 255
    , themeBorder = colorRGBA 176 176 172 255
    , themeText = colorRGBA 34 34 36 255
    , themeTextDim = colorRGBA 120 120 118 255
    , themeAccent = colorRGBA 34 102 190 255
    , themeAccentText = colorRGBA 255 255 255 255
    , themeRowAlt = colorRGBA 240 240 237 255
    , themeRowHover = colorRGBA 226 226 222 255
    , themeGap = 6
    , themeWindowPad = 10
    , themeTextAlign = AlignMiddle
    }

-- | Space between a widget's box and its text.
widgetPad :: Float
widgetPad = 6

-- | Space between a field's box and its text.
fieldPad :: Float
fieldPad = 4

-- | The height of a one-line field: a line of text, its padding and 1px
-- borders. Captions aligned with 'alignTextToFrame' take it too.
fieldHeight :: Float
fieldHeight = lineHeight + fieldPad * 2 + 2

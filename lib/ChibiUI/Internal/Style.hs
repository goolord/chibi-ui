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

import ChibiUI.Internal.Font (lineHeight)
import ChibiUI.Internal.Types (Color, Rect (..), colorRGBA)

data TextAlign = AlignTop | AlignMiddle | AlignBottom
  deriving stock (Eq, Show)

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
  -- ^ Space between the window edge and the first widget.
  , themeTextAlign :: !TextAlign
  -- ^ Where a line of text sits in a box taller than it.
  }
  deriving stock (Eq, Show)

-- | A dark theme.
defaultTheme :: Theme
defaultTheme =
  Theme
    { themeWindow = colorRGBA 28 29 33 255
    , themeSurface = colorRGBA 38 39 44 255
    , themeSurfaceHover = colorRGBA 47 49 55 255
    , themeSurfaceActive = colorRGBA 57 60 67 255
    , themeBorder = colorRGBA 66 68 77 255
    , themeText = colorRGBA 227 228 232 255
    , themeTextDim = colorRGBA 150 153 163 255
    , themeAccent = colorRGBA 110 160 245 255
    , themeAccentText = colorRGBA 16 18 24 255
    , themeRowAlt = colorRGBA 33 34 39 255
    , themeRowHover = colorRGBA 44 46 52 255
    , themeGap = 8
    , themeWindowPad = 10
    , themeTextAlign = AlignMiddle
    }

-- | A light theme: the dark theme's spacing in light colours.
lightTheme :: Theme
lightTheme =
  defaultTheme
    { themeWindow = colorRGBA 246 246 248 255
    , themeSurface = colorRGBA 255 255 255 255
    , themeSurfaceHover = colorRGBA 238 239 242 255
    , themeSurfaceActive = colorRGBA 225 227 233 255
    , themeBorder = colorRGBA 192 195 203 255
    , themeText = colorRGBA 28 30 36 255
    , themeTextDim = colorRGBA 102 106 116 255
    , themeAccent = colorRGBA 37 99 205 255
    , themeAccentText = colorRGBA 255 255 255 255
    , themeRowAlt = colorRGBA 241 242 245 255
    , themeRowHover = colorRGBA 230 232 237 255
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

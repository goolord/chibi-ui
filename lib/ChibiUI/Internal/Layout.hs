-- | Cursor layout: one cursor per frame advances as widgets are declared.
-- A widget takes the rectangle at the cursor, then the cursor steps past it
-- down by default, rightward once @sameLine@ or a @row@ opened the line.
-- There are no layout nodes and no solver; what a frame declares is where
-- it lands.
module ChibiUI.Internal.Layout
  ( LayoutState (..)
  , freshLayout
  , LayoutCommand (..)
  , stepLayout
  , placeLayout
  , remainingWidth
  , beginGroup
  , contentSize
  , beginViewport
  , beginIndent
  , endIndent
  ) where

import Data.Maybe (fromMaybe)
import ChibiUI.Internal.Types (Rect (..), Size (..), V2 (..), rectUnion)

-- | Where the cursor is and what the next widget inherits.
data LayoutState = LayoutState
  { lsPenX :: {-# UNPACK #-} !Float
  -- ^ The next widget's left edge, in window coordinates.
  , lsLineY :: {-# UNPACK #-} !Float
  -- ^ The current line's top edge. A widget placed on a fresh line lands
  -- here; the line then advances past it.
  , lsLineH :: {-# UNPACK #-} !Float
  -- ^ The tallest widget placed on the current line, for stepping down
  -- past it and for a @row@ to size itself by.
  , lsRowOpen :: !Bool
  -- ^ Whether the line is open for more widgets ('sameLine' or a @row@
  -- opened it). While it is, widgets advance rightward; closing it steps
  -- down by the line's height.
  , lsFlowRow :: !Bool
  -- ^ A row scope keeps advancing horizontally without 'sameLine'.
  , lsLast :: {-# UNPACK #-} !Rect
  -- ^ The last placed widget's rect, for 'sameLine'.
  , lsIndent :: {-# UNPACK #-} !Float
  -- ^ Where lines start, moved right by @indent@.
  , lsNextW :: !(Maybe Float)
  -- ^ A one-shot width the next widget takes instead of measuring.
  , lsNextH :: !(Maybe Float)
  -- ^ A one-shot height for the next widget.
  , lsAvailW :: {-# UNPACK #-} !Float
  -- ^ Width from the indentation to the right edge of this scope.
  , lsBounded :: !Bool
  -- ^ Whether 'lsBounds' holds an item placed in this scope.
  , lsBounds :: {-# UNPACK #-} !Rect
  -- ^ Bounds of all items placed in this scope, excluding trailing gaps.
  }
  deriving (Eq, Show)

-- | The cursor at the window's origin, stepping down.
freshLayout :: LayoutState
freshLayout =
  LayoutState
    { lsPenX = 0
    , lsLineY = 0
    , lsLineH = 0
    , lsRowOpen = False
    , lsFlowRow = False
    , lsLast = Rect 0 0 0 0
    , lsIndent = 0
    , lsNextW = Nothing
    , lsNextH = Nothing
    , lsAvailW = 0
    , lsBounded = False
    , lsBounds = Rect 0 0 0 0
    }

-- | Commands change the cursor without contributing widget bounds.
data LayoutCommand = SameLine | Newline | NextWidth !Float | NextHeight !Float | Space !Float
  deriving (Eq, Show)

stepLayout :: Float -> LayoutCommand -> LayoutState -> LayoutState
stepLayout gap command ls = case command of
  SameLine -> ls {lsRowOpen = True, lsPenX = rectX r + rectW r + gap, lsLineY = rectY r}
  Newline -> ls
    { lsRowOpen = lsFlowRow ls
    , lsPenX = lsIndent ls
    , lsLineY = if lsRowOpen ls then lsLineY ls + lsLineH ls + gap else lsLineY ls
    , lsLineH = 0
    }
  NextWidth w -> ls {lsNextW = Just w}
  NextHeight h -> ls {lsNextH = Just h}
  Space n | lsRowOpen ls -> ls {lsPenX = lsPenX ls + n}
          | otherwise -> ls {lsLineY = lsLineY ls + n}
  where r = lsLast ls

-- | Consume size overrides once, accumulate bounds, and advance past the item.
placeLayout :: Float -> Size -> LayoutState -> (Rect, LayoutState)
placeLayout gap sz ls = (r, ls
  { lsPenX = if lsRowOpen ls
      then if continuesRow then rectX r + w + gap else lsIndent ls
      else lsPenX ls
  , lsLineY = if continuesRow then rectY r else rectY r + lineH + gap
  , lsRowOpen = continuesRow
  , lsLineH = lineH
  , lsLast = r
  , lsNextW = Nothing
  , lsNextH = Nothing
  , lsBounded = True
  , lsBounds = if lsBounded ls then rectUnion r (lsBounds ls) else r
  })
  where
    w = max 0 (fromMaybe (sizeW sz) (lsNextW ls))
    h = max 0 (fromMaybe (sizeH sz) (lsNextH ls))
    r = Rect (lsPenX ls) (lsLineY ls) w h
    lineH = if lsRowOpen ls then max (lsLineH ls) h else h
    continuesRow = lsRowOpen ls && lsFlowRow ls

remainingWidth :: LayoutState -> Float
remainingWidth ls = max 0 (lsIndent ls + lsAvailW ls - lsPenX ls)

-- | A group measures its children independently, then occupies one parent item.
beginGroup :: Bool -> LayoutState -> LayoutState
beginGroup horizontal ls = ls
  { lsRowOpen = horizontal
  , lsFlowRow = horizontal
  , lsIndent = lsPenX ls
  , lsAvailW = max 0 (fromMaybe (remainingWidth ls) (lsNextW ls))
  , lsLineH = 0
  , lsLast = Rect (lsPenX ls) (lsLineY ls) 0 0
  , lsNextW = Nothing
  , lsNextH = Nothing
  , lsBounded = False
  , lsBounds = Rect 0 0 0 0
  }

-- | Extent from a scope's origin, excluding trailing cursor spacing.
contentSize :: V2 -> LayoutState -> Size
contentSize (V2 x y) child
  | not (lsBounded child) = Size 0 0
  | otherwise =
      let b = lsBounds child
       in Size (max 0 (rectX b + rectW b - x)) (max 0 (rectY b + rectH b - y))

-- | Lay out scrolling content in its own shifted, vertical coordinate space.
beginViewport :: Rect -> LayoutState -> LayoutState
beginViewport r ls = ls
  { lsPenX = rectX r
  , lsLineY = rectY r
  , lsIndent = rectX r
  , lsAvailW = max 0 (rectW r)
  , lsRowOpen = False
  , lsFlowRow = False
  , lsLineH = 0
  , lsBounded = False
  , lsBounds = Rect 0 0 0 0
  , lsLast = Rect (rectX r) (rectY r) 0 0
  }

beginIndent :: Float -> LayoutState -> LayoutState
beginIndent n ls = ls
  { lsIndent = lsIndent ls + n, lsPenX = lsPenX ls + n, lsAvailW = max 0 (lsAvailW ls - n) }

endIndent :: LayoutState -> LayoutState -> LayoutState
endIndent parent child = child
  { lsIndent = lsIndent parent
  , lsAvailW = lsAvailW parent
  , lsPenX = if lsRowOpen child then lsPenX child else lsIndent parent
  }

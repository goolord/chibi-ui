-- | Cursor layout: one cursor per frame advances as widgets are declared.
-- A widget takes the rectangle at the cursor, then the cursor steps past it
-- down by default, rightward once @sameLine@ or a @row@ opened the line.
-- There are no layout nodes and no solver; what a frame declares is where
-- it lands.
module ChibiUI.Internal.Layout
  ( LayoutState (..)
  , LineMode (..)
  , freshLayout
  , LayoutCommand (..)
  , stepLayout
  , placeLayout
  , remainingWidth
  , nextOrRemainingWidth
  , remainingHeight
  , beginGroup
  , endGroup
  , contentSize
  , beginViewport
  , beginIndent
  , endIndent
  ) where

import Data.Maybe (fromMaybe)
import ChibiUI.Internal.Types (Rect (..), Size (..), V2 (..), rectUnion)

-- | How the next widget joins the current line.
data LineMode
  = Stacked
  -- ^ Below the line: each widget steps the cursor down past itself.
  | JoinOnce
  -- ^ Beside the last widget, once: 'sameLine' opened the line, and the
  -- next widget placed closes it again.
  | Flowing
  -- ^ Beside the last widget, always: a @row@'s children flow rightward,
  -- so 'sameLine' there changes nothing.
  deriving (Eq, Show)

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
  , lsMode :: !LineMode
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
  , lsBottom :: {-# UNPACK #-} !Float
  -- ^ This scope's bottom edge, in window coordinates.
  , lsBounded :: !Bool
  -- ^ Whether 'lsBounds' holds an item placed in this scope.
  , lsBounds :: {-# UNPACK #-} !Rect
  -- ^ Bounds of all items placed in this scope, excluding trailing gaps.
  }
  deriving (Eq, Show)

-- | Whether the line is open for more widgets beside the last.
lineOpen :: LayoutState -> Bool
lineOpen ls = lsMode ls /= Stacked

-- | An empty scope with its cursor at @(x, y)@, @w@ wide and reaching down
-- to @bottom@: every scope a layout starts is one.
scopeAt :: LineMode -> Float -> Float -> Float -> Float -> LayoutState
scopeAt mode x y w bottom =
  LayoutState
    { lsPenX = x
    , lsLineY = y
    , lsLineH = 0
    , lsMode = mode
    , lsLast = Rect x y 0 0
    , lsIndent = x
    , lsNextW = Nothing
    , lsNextH = Nothing
    , lsAvailW = max 0 w
    , lsBottom = bottom
    , lsBounded = False
    , lsBounds = Rect 0 0 0 0
    }

-- | The cursor at the window's origin, stepping down.
freshLayout :: LayoutState
freshLayout = scopeAt Stacked 0 0 0 0

-- | Commands change the cursor without contributing widget bounds.
data LayoutCommand = SameLine | Newline | NextWidth !Float | NextHeight !Float | Space !Float
  deriving (Eq, Show)

stepLayout :: Float -> LayoutCommand -> LayoutState -> LayoutState
stepLayout gap command ls = case command of
  SameLine -> ls
    { lsMode = if lsMode ls == Flowing then Flowing else JoinOnce
    , lsPenX = rectX r + rectW r + gap
    , lsLineY = rectY r
    }
  Newline -> ls
    { lsMode = if lsMode ls == Flowing then Flowing else Stacked
    , lsPenX = lsIndent ls
    , lsLineY = if lineOpen ls then lsLineY ls + lsLineH ls + gap else lsLineY ls
    , lsLineH = 0
    }
  NextWidth w -> ls {lsNextW = Just w}
  NextHeight h -> ls {lsNextH = Just h}
  Space n | lineOpen ls -> ls {lsPenX = lsPenX ls + n}
          | otherwise -> ls {lsLineY = lsLineY ls + n}
  where r = lsLast ls

-- | Consume size overrides once, accumulate bounds, and advance past the item.
placeLayout :: Float -> Size -> LayoutState -> (Rect, LayoutState)
placeLayout gap sz ls = (r, ls
  { lsPenX = case lsMode ls of
      Flowing -> rectX r + w + gap
      JoinOnce -> lsIndent ls
      Stacked -> lsPenX ls
  , lsLineY = if flowing then rectY r else rectY r + lineH + gap
  , lsMode = if flowing then Flowing else Stacked
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
    lineH = if lineOpen ls then max (lsLineH ls) h else h
    flowing = lsMode ls == Flowing

remainingWidth :: LayoutState -> Float
remainingWidth ls = max 0 (lsIndent ls + lsAvailW ls - lsPenX ls)

-- | The width the next widget will get: a pending 'NextWidth', or the
-- rest of the line.
nextOrRemainingWidth :: LayoutState -> Float
nextOrRemainingWidth ls = fromMaybe (remainingWidth ls) (lsNextW ls)

remainingHeight :: LayoutState -> Float
remainingHeight ls = max 0 (lsBottom ls - lsLineY ls)

-- | A group measures its children independently, then occupies one parent item.
beginGroup :: Bool -> LayoutState -> LayoutState
beginGroup horizontal ls =
  scopeAt (if horizontal then Flowing else Stacked) (lsPenX ls) (lsLineY ls)
    (nextOrRemainingWidth ls)
    (maybe (lsBottom ls) (lsLineY ls +) (lsNextH ls))

-- | Leave a group: its size, from where it began in the parent, and the
-- parent's cursor back, for the group to be placed as one item.
endGroup :: LayoutState -> LayoutState -> (Size, LayoutState)
endGroup parent child = (contentSize (V2 (lsPenX parent) (lsLineY parent)) child, parent)

-- | Extent from a scope's origin, excluding trailing cursor spacing.
contentSize :: V2 -> LayoutState -> Size
contentSize (V2 x y) child
  | not (lsBounded child) = Size 0 0
  | otherwise =
      let b = lsBounds child
       in Size (max 0 (rectX b + rectW b - x)) (max 0 (rectY b + rectH b - y))

-- | Lay out scrolling content in its own shifted, vertical coordinate
-- space. Its bottom is one viewport below its top.
beginViewport :: Rect -> LayoutState
beginViewport (Rect x y w h) = scopeAt Stacked x y w (y + h)

beginIndent :: Float -> LayoutState -> LayoutState
beginIndent n ls = ls
  { lsIndent = lsIndent ls + n, lsPenX = lsPenX ls + n, lsAvailW = max 0 (lsAvailW ls - n) }

endIndent :: LayoutState -> LayoutState -> LayoutState
endIndent parent child = child
  { lsIndent = lsIndent parent
  , lsAvailW = lsAvailW parent
  , lsPenX = if lineOpen child then lsPenX child else lsIndent parent
  }

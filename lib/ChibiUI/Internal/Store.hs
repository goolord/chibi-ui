-- | Transient widget state, keyed by widget identity. Application state
-- lives separately in the context's user-defined model.
module ChibiUI.Internal.Store
  ( WidgetStore (..)
  , emptyWidgetStore
  , Field
  , fieldInt
  , fieldFloat
  , fieldDyn
  , overField
  , lookupSlot
  , findSlot
  , memberSlot
  , insertSlot
  , deleteSlot
  , Slot (..)
  , slotKey
  ) where

import Data.Dynamic (Dynamic)
import Data.IntMap.Strict (IntMap)
import qualified Data.IntMap.Strict as IM
import ChibiUI.Internal.Id (mix64)

-- | Widget state for every widget, in maps by value type, keyed by
-- @fromIntegral (hashWidgetId wid)@. Same-type fields that share a widget
-- key use 'slotKey'.
data WidgetStore = WidgetStore
  { storeInt :: !(IntMap Int)
  , storeFloat :: !(IntMap Float)
  , storeDyn :: !(IntMap Dynamic)
  }

-- | One of the store's maps: how to read it, and how to put a new one back.
-- The slot functions inline at the field they are given, so
-- @insertSlot fieldInt k v@ compiles to the record update it stands for.
data Field a = Field (WidgetStore -> IntMap a) (IntMap a -> WidgetStore -> WidgetStore)

-- | Integer slots, for table selection.
fieldInt :: Field Int
fieldInt = Field storeInt (\m st -> st {storeInt = m})

-- | Single-precision numeric slots.
fieldFloat :: Field Float
fieldFloat = Field storeFloat (\m st -> st {storeFloat = m})

-- | Runtime-typed slots, for editor history, pointer clicks and queued commands.
fieldDyn :: Field Dynamic
fieldDyn = Field storeDyn (\m st -> st {storeDyn = m})

-- | Read the map selected by a field descriptor.
{-# INLINE fieldMap #-}
fieldMap :: Field a -> WidgetStore -> IntMap a
fieldMap (Field get _) = get

-- | Pure update of one selected map.
{-# INLINE overField #-}
overField :: Field a -> (IntMap a -> IntMap a) -> WidgetStore -> WidgetStore
overField (Field get set) f st = set (f (get st)) st

-- | Read a key from a typed map, returning 'Nothing' when absent.
{-# INLINE lookupSlot #-}
lookupSlot :: Field a -> Int -> WidgetStore -> Maybe a
lookupSlot field k = IM.lookup k . fieldMap field

-- | The slot's value, or @def@ while it has none.
{-# INLINE findSlot #-}
findSlot :: Field a -> a -> Int -> WidgetStore -> a
findSlot field def k = IM.findWithDefault def k . fieldMap field

-- | Whether a key exists in the selected map, regardless of its value.
{-# INLINE memberSlot #-}
memberSlot :: Field a -> Int -> WidgetStore -> Bool
memberSlot field k = IM.member k . fieldMap field

-- | Pure insert or replacement.
{-# INLINE insertSlot #-}
insertSlot :: Field a -> Int -> a -> WidgetStore -> WidgetStore
insertSlot field k v = overField field (IM.insert k v)

-- | Pure removal of a key; an absent key leaves the map unchanged.
{-# INLINE deleteSlot #-}
deleteSlot :: Field a -> Int -> WidgetStore -> WidgetStore
deleteSlot field k = overField field (IM.delete k)

-- | Every built-in slot.
data Slot
  = -- | A text field's caret, in characters.
    SlotCursor
  | -- | A scroll region's y offset.
    SlotScrollY
  | -- | A scroll region's content extent.
    SlotScrollExtent
  | -- | A table's selected row index, plus one; 0 is none.
    SlotTableSel
  | -- | A numeric field's held stepper direction: 1 up, -1 down.
    SlotNumericHeld
  | SlotTextScroll
  | SlotTextClick
  | SlotTextCommand
  | SlotTreeOpen
  | SlotTextScrollY
  deriving (Enum)

-- | Tag for a built-in slot under one widget's key: the constructor index
-- mixed with a salt, so tags are well spread.
{-# INLINE slotKey #-}
slotKey :: Slot -> Int -> Int
slotKey s k = fromIntegral (mix64 (fromIntegral k) (mix64 0x534C4F5454414753 (fromIntegral (fromEnum s))))

-- | Empty maps.
emptyWidgetStore :: WidgetStore
emptyWidgetStore =
  WidgetStore
    { storeInt = IM.empty
    , storeFloat = IM.empty
    , storeDyn = IM.empty
    }

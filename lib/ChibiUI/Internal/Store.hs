-- | Transient widget state, keyed by widget identity, in one concretely
-- typed map per kind of state. Application state lives separately in the
-- context's model.
module ChibiUI.Internal.Store
  ( WidgetStore
  , emptyWidgetStore
  , StoreMap
  , treeOpen
  , tableSelection
  , scrollRegions
  , textFields
  , lookupState
  , writeState
  -- * State
  , ScrollState (..)
  , FieldState (..)
  , Draft (..)
  , ClickState (..)
  ) where

import Data.IntMap.Strict (IntMap)
import qualified Data.IntMap.Strict as IM
import Data.Text (Text)
import ChibiUI.Internal.Editor (Command, Editor)
import ChibiUI.Internal.Types (V2)

-- | Every widget's state, keyed by @fromIntegral (hashWidgetId wid)@. A
-- widget has at most one entry, in the map for its kind.
data WidgetStore = WidgetStore
  { storeTreeOpen :: !(IntMap ())
  , storeTableSelection :: !(IntMap Int)
  , storeScroll :: !(IntMap ScrollState)
  , storeTextField :: !(IntMap FieldState)
  }

-- | No widget has state.
emptyWidgetStore :: WidgetStore
emptyWidgetStore = WidgetStore IM.empty IM.empty IM.empty IM.empty

-- | One of the store's maps: how to read it, and how to put a new one back.
-- The accessors inline at the map they are given, so a write compiles to
-- the record update it stands for.
data StoreMap a = StoreMap (WidgetStore -> IntMap a) (IntMap a -> WidgetStore -> WidgetStore)

-- | The tree nodes that are open; a closed node has no entry.
treeOpen :: StoreMap ()
treeOpen = StoreMap storeTreeOpen (\m st -> st {storeTreeOpen = m})

-- | Each table's selected row; a table with none has no entry.
tableSelection :: StoreMap Int
tableSelection = StoreMap storeTableSelection (\m st -> st {storeTableSelection = m})

-- | Each scroll region's offset and body extent.
scrollRegions :: StoreMap ScrollState
scrollRegions = StoreMap storeScroll (\m st -> st {storeScroll = m})

-- | Each text field's draft, scroll and pointer state.
textFields :: StoreMap FieldState
textFields = StoreMap storeTextField (\m st -> st {storeTextField = m})

-- | A widget's entry in one map.
{-# INLINE lookupState #-}
lookupState :: StoreMap a -> Int -> WidgetStore -> Maybe a
lookupState (StoreMap get _) k = IM.lookup k . get

-- | Replace ('Just') or remove ('Nothing') a widget's entry in one map.
{-# INLINE writeState #-}
writeState :: StoreMap a -> Int -> Maybe a -> WidgetStore -> WidgetStore
writeState (StoreMap get set) k v st = set (maybe (IM.delete k) (IM.insert k) v (get st)) st

-- | A scroll region's offset, and the extent of its body when last run.
data ScrollState = ScrollState !Float !Float
  deriving (Eq)

-- | What a text field keeps between frames.
data FieldState = FieldState
  { fieldDraft :: !Draft
  , fieldScroll :: !V2
  -- ^ How far the text is scrolled; back to the start when an edit ends.
  , fieldClick :: !(Maybe ClickState)
  -- ^ The last press, for counting double and triple clicks.
  , fieldQueued :: !(Maybe Command)
  -- ^ A command the field's menu chose, run on the next frame.
  }
  deriving (Eq)

-- | An inactive editor retains undo history; an editing session also owns
-- the focus-time value. The draft and its cancellation target cannot drift
-- apart.
data Draft = Inactive !Editor | Editing !Text !Editor
  deriving (Eq)

-- | A press on a text field: when, where, and how many presses in a row.
data ClickState = ClickState !Double !V2 !Int
  deriving (Eq)

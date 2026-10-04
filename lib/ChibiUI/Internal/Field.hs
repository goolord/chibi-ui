{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

-- | Lenses onto record fields by name, derived from 'Generic'. The
-- @#field@ labels in "ChibiUI.Labels" are these lenses.
module ChibiUI.Internal.Field
  ( field
  , GField
  ) where

import Data.Kind (Type)
import Data.Type.Bool (type (||))
import GHC.Generics
import GHC.TypeLits (ErrorMessage (..), Symbol, TypeError)
import Lens.Micro (Lens')

-- | A lens onto the field called @name@ of a single-constructor record.
--
-- > data Model = Model { count :: Int } deriving Generic
-- >
-- > mapModel (field @"count") counter
{-# INLINE field #-}
field :: forall name s a. (Generic s, GField name (Rep s) a) => Lens' s a
field f s = to <$> gfield @name f (from s)

-- | The field @name@ somewhere in a generic representation. The field's
-- type follows from the representation alone, so @a@ need not be known.
class GField (name :: Symbol) (rep :: Type -> Type) a where
  gfield :: Lens' (rep x) a

instance GField name f a => GField name (D1 m f) a where
  {-# INLINE gfield #-}
  gfield f (M1 x) = M1 <$> gfield @name f x

instance GField name f a => GField name (C1 m f) a where
  {-# INLINE gfield #-}
  gfield f (M1 x) = M1 <$> gfield @name f x

instance GSide (Has name l) name l r a => GField name (l :*: r) a where
  {-# INLINE gfield #-}
  gfield = gside @(Has name l) @name

instance a ~ b => GField name (S1 ('MetaSel ('Just name) u s l) (K1 i b)) a where
  {-# INLINE gfield #-}
  gfield f (M1 (K1 x)) = M1 . K1 <$> f x

instance {-# OVERLAPPABLE #-}
  TypeError ('Text "The record has no field " ':<>: 'ShowType name)
  => GField name (S1 m k) a where
  gfield = error "unreachable"

instance
  TypeError ('Text "Cannot focus field " ':<>: 'ShowType name
    ':<>: 'Text " of a type with more than one constructor")
  => GField name (l :+: r) a where
  gfield = error "unreachable"

-- | Whether a product holds the field @name@.
type family Has (name :: Symbol) (rep :: Type -> Type) :: Bool where
  Has name (S1 ('MetaSel ('Just name) u s l) k) = 'True
  Has name (l :*: r) = Has name l || Has name r
  Has _ _ = 'False

-- | Descend into the side of a product that holds the field: the left
-- when it does, otherwise the right.
class GSide (left :: Bool) (name :: Symbol) l r a where
  gside :: Lens' ((l :*: r) x) a

instance GField name l a => GSide 'True name l r a where
  {-# INLINE gside #-}
  gside f (l :*: r) = (:*: r) <$> gfield @name f l

instance GField name r a => GSide 'False name l r a where
  {-# INLINE gside #-}
  gside f (l :*: r) = (l :*:) <$> gfield @name f r

{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

-- | Record fields as lenses through @OverloadedLabels@: with this module
-- imported, @#count@ is @'field' \@"count"@, a lens onto the @count@
-- field of any record that derives 'GHC.Generics.Generic'.
--
-- > {-# LANGUAGE OverloadedLabels #-}
-- > import ChibiUI
-- > import ChibiUI.Labels ()
-- >
-- > data Model = Model { name :: Text, count :: Int } deriving Generic
-- >
-- > view :: ChibiUI Model ()
-- > view = do
-- >   _ <- edit #name textInput
-- >   mapModel #count counter
--
-- The labels are ordinary van Laarhoven lenses, so microlens and lens
-- operators take them too: @modify (over #count (+ 1))@, or with
-- "Lens.Micro.Mtl", @#count %= (+ 1)@ and @use #count@.
--
-- The instance is on function types, as the one in generic-lens's
-- @Data.Generics.Labels@ is, so the two cannot both be in scope.
module ChibiUI.Labels (field) where

import GHC.Generics (Generic (..))
import GHC.OverloadedLabels (IsLabel (..))
import ChibiUI.Internal.Field (GField, field)

instance (Functor f, Generic s, GField name (Rep s) a, a ~ b, s ~ t)
  => IsLabel name ((a -> f b) -> s -> f t) where
  {-# INLINE fromLabel #-}
  fromLabel = field @name

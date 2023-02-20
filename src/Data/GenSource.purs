module Data.GenSource
  ( arbitraryIn
  , class GenSource
  , members
  ) where

import Prelude

import Control.Monad.Gen (class MonadGen, elements)
import Data.Array.NonEmpty (NonEmptyArray, fromFoldable1, toUnfoldable1)
import Data.Set.NonEmpty (NonEmptySet)
import Data.String.NonEmpty (NonEmptyString)
import Data.String.NonEmpty.CodeUnits (toNonEmptyCharArray)

class GenSource src sample | src -> sample where
  members :: src -> NonEmptyArray sample
  arbitraryIn :: forall m. MonadGen m => src -> m sample

instance genSourceNonEmptyArray :: GenSource (NonEmptyArray a) a where
  members = identity
  arbitraryIn = elements <<< members

else instance genSourceNonEmptySet :: GenSource (NonEmptySet a) a where
  members = toUnfoldable1 <<< fromFoldable1
  arbitraryIn = arbitraryIn <<< members

else instance genSourceNonEmptyString :: GenSource NonEmptyString Char where
  members = toNonEmptyCharArray
  arbitraryIn = arbitraryIn <<< members

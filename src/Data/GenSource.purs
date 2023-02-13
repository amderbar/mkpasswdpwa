module Data.GenSource
  ( arbitraryIn
  , class GenSource
  , members
  )
  where

import Prelude

import Control.Monad.Gen (class MonadGen, elements)
import Data.Array.NonEmpty (NonEmptyArray)
import Data.Char.Subset (class SubsetChar, toChar)
import Data.String.NonEmpty (NonEmptyString)
import Data.String.NonEmpty.CodeUnits (toNonEmptyCharArray)

class GenSource src sample | src -> sample where
  members :: src -> NonEmptyArray sample
  arbitraryIn :: forall m. MonadGen m => src -> m sample

instance genSourceNonEmptyCharArray :: SubsetChar c => GenSource (NonEmptyArray c) Char where
  members charset = toChar <$> charset
  arbitraryIn = elements <<< members

else instance genSourceNonEmptyString :: GenSource NonEmptyString Char where
  members = toNonEmptyCharArray
  arbitraryIn = arbitraryIn <<< members

else instance genSourceNonEmptyArray :: GenSource (NonEmptyArray a) a where
  members = identity
  arbitraryIn = elements

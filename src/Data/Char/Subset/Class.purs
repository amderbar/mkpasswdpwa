module Data.Char.Subset.Class where

import Prelude

import Control.Monad.Error.Class (class MonadError)
import Data.Array (fromFoldable, toUnfoldable)
import Data.Array.NonEmpty (fromFoldable1, toUnfoldable1)
import Data.Semigroup.Foldable (class Foldable1)
import Data.String.CodeUnits (fromCharArray, toCharArray)
import Data.String.NonEmpty (NonEmptyString)
import Data.String.NonEmpty.CodeUnits (fromNonEmptyCharArray, toNonEmptyCharArray)
import Data.Traversable (class Foldable, traverse)
import Data.Unfoldable (class Unfoldable, class Unfoldable1)

class SubsetChar char where
  fromChar :: forall e m. MonadError e m => e -> Char -> m char
  toChar :: char -> Char

instance subsetCharChar :: SubsetChar Char where
  fromChar _ = pure
  toChar = identity

fromString :: forall e m f c. MonadError e m => Unfoldable f => SubsetChar c => (Char -> e) -> String -> m (f c)
fromString e = toCharArray >>> traverse (\c -> fromChar (e c) c) >>> map toUnfoldable

toString :: forall f c. Foldable f => SubsetChar c => f c -> String
toString = fromFoldable >>> (map toChar) >>> fromCharArray

fromNonEmptyString :: forall e m f c. MonadError e m => Unfoldable1 f => SubsetChar c => (Char -> e) -> NonEmptyString -> m (f c)
fromNonEmptyString e = toNonEmptyCharArray >>> traverse (\c -> fromChar (e c) c) >>> map toUnfoldable1

toNonEmptyString :: forall f c. Foldable1 f => SubsetChar c => f c -> NonEmptyString
toNonEmptyString = fromFoldable1 >>> (map toChar) >>> fromNonEmptyCharArray

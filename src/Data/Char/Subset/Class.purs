module Data.Char.Subset.Class where

import Prelude

import Control.Monad.Error.Class (class MonadError)
import Data.String.CodeUnits (fromCharArray, toCharArray)
import Data.Traversable (traverse)

class SubsetChar char where
  fromChar :: forall e m. MonadError e m => e -> Char -> m char
  toChar :: char -> Char

instance subsetCharChar :: SubsetChar Char where
  fromChar _ = pure
  toChar = identity

fromString :: forall e m c. MonadError e m => SubsetChar c => (Char -> e) -> String -> m (Array c)
fromString e = toCharArray >>> traverse \c -> fromChar (e c) c

toString :: forall c. SubsetChar c => Array c -> String
toString = (map toChar) >>> fromCharArray

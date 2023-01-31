module Data.Char.Subset.Class where

import Prelude

import Data.Array.NonEmpty (NonEmptyArray)
import Data.Maybe (Maybe(..))
import Data.String.NonEmpty (fromString) as Str
import Data.String.NonEmpty.CodeUnits (toNonEmptyCharArray)

class SubsetChar char where
  fromChar :: Char -> Maybe char
  toChar :: char -> Char
  fromString :: String -> Maybe (NonEmptyArray char)

instance subsetCharChar :: SubsetChar Char where
  fromChar = Just
  toChar = identity
  fromString = Str.fromString >>> (map toNonEmptyCharArray)

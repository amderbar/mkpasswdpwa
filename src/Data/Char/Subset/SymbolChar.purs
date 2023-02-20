module Data.Char.Subset.SymbolChar where

import Prelude

import Control.Alternative (guard)
import Control.Monad.Error.Class (liftMaybe)
import Data.Array.NonEmpty (NonEmptyArray, elem)
import Data.Char.Subset.Class (class SubsetChar)
import Data.Maybe (Maybe)
import Data.String.NonEmpty (nes) as Str
import Data.String.NonEmpty.CodeUnits (toNonEmptyCharArray)
import Type.Proxy (Proxy(..))

newtype SymbolChar = SymbolChar Char

derive instance eqSymbolChar :: Eq SymbolChar

derive newtype instance showPasswd :: Show SymbolChar

instance subsetCharSymbolChar :: SubsetChar SymbolChar where
  fromChar e = toSymbolChar >>> liftMaybe e
  toChar (SymbolChar c) = c

symbols :: NonEmptyArray SymbolChar
symbols =
  SymbolChar <$> (toNonEmptyCharArray $ Str.nes allSymbol)
  where
  allSymbol = Proxy :: Proxy "!\"#$%&'()*+,-./:;<=>?@[\\]^_`{|}~"

toSymbolChar :: Char -> Maybe SymbolChar
toSymbolChar c = do
  let s = SymbolChar c
  guard (s `elem` symbols)
  pure s

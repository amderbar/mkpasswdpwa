module Data.Char.Subset.Hiragana where

import Prelude

import Control.Alternative (guard)
import Control.Monad.Error.Class (liftMaybe)
import Data.Array.NonEmpty (NonEmptyArray, elem)
import Data.Char.Subset.Class (class SubsetChar)
import Data.Maybe (Maybe)
import Data.String.NonEmpty (nes) as Str
import Data.String.NonEmpty.CodeUnits (toNonEmptyCharArray)
import Type.Proxy (Proxy(Proxy))

newtype Hiragana = Hiragana Char

derive instance eqHiragana :: Eq Hiragana

derive newtype instance showHiragana :: Show Hiragana

instance subsetCharHiragana :: SubsetChar Hiragana where
  fromChar e = toHiragana >>> liftMaybe e
  toChar (Hiragana c) = c

hiragana :: NonEmptyArray Hiragana
hiragana = Hiragana <$> (toNonEmptyCharArray $ Str.nes allHiragana)
  where
  allHiragana = Proxy :: Proxy "あいうえおかきくけこさしすせそたちつてとなにぬねのはひふへほまみむめもやゆよらりるれろわをん"

toHiragana :: Char -> Maybe Hiragana
toHiragana c = do
  let s = Hiragana c
  guard (s `elem` hiragana)
  pure s

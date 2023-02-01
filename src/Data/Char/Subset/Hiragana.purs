module Data.Char.Subset.Hiragana where

import Prelude

import Control.Alternative (guard)
import Control.Monad.Gen (elements)
import Data.Array.NonEmpty (NonEmptyArray, elem)
import Data.Char.Subset.Class (class SubsetChar)
import Data.Either (Either, note, hush)
import Data.Maybe (Maybe)
import Data.String.NonEmpty (fromString, nes) as Str
import Data.String.NonEmpty.CodeUnits (toNonEmptyCharArray)
import Data.Traversable (traverse)
import Test.QuickCheck (class Arbitrary)
import Type.Proxy (Proxy(Proxy))

newtype Hiragana
  = Hiragana Char

derive instance eqHiragana :: Eq Hiragana

derive newtype instance showHiragana :: Show Hiragana

instance arbitaryHiragana :: Arbitrary Hiragana where
  arbitrary = elements hiragana

instance subsetCharHiragana :: SubsetChar Hiragana where
  fromChar = toHiragana
  toChar (Hiragana c) = c
  fromString = toNonEmptyHiraganaArrray >>> hush

hiragana :: NonEmptyArray Hiragana
hiragana
 = Hiragana <$> (toNonEmptyCharArray $ Str.nes allHiragana)
  where
    allHiragana = Proxy :: Proxy "あいうえおかきくけこさしすせそたちつてとなにぬねのはひふへほまみむめもやゆよらりるれろわをん"

toHiragana :: Char -> Maybe Hiragana
toHiragana c = do
  let s = Hiragana c
  guard (s `elem` hiragana)
  pure s

toNonEmptyHiraganaArrray :: String -> Either String (NonEmptyArray Hiragana)
toNonEmptyHiraganaArrray = fromString >=> traverse toHiraganaE
  where
  fromString =
    Str.fromString
      >>> (map toNonEmptyCharArray)
      >>> note "charset should be non empty"

  toHiraganaE c = note ("Invalid Hiragana:" <> show c) $ toHiragana c

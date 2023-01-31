module Data.Char.Subset.SymbolChar where

import Prelude

import Control.Monad.Gen (class MonadGen, elements)
import Data.Array (concat, (..), catMaybes)
import Data.Array.NonEmpty (NonEmptyArray, appendArray, singleton, elem)
import Data.Char (fromCharCode)
import Data.Char.Subset.Class (class SubsetChar)
import Data.Either (Either, note, hush)
import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype)
import Data.String.NonEmpty (fromString) as Str
import Data.String.NonEmpty.CodeUnits (toNonEmptyCharArray)
import Data.Traversable (traverse)
import Test.QuickCheck (class Arbitrary)

newtype SymbolChar
  = SymbolChar Char

derive instance eqSymbolChar :: Eq SymbolChar

derive instance newtypePasswd :: Newtype SymbolChar _

derive newtype instance showPasswd :: Show SymbolChar

instance arbitarySymbolChar :: Arbitrary SymbolChar where
  arbitrary = genSymbols

instance subsetCharSymbolChar :: SubsetChar SymbolChar where
  fromChar = toSymbolChar
  toChar (SymbolChar c) = c
  fromString = toNonEmptySymbolCharArrray >>> hush

symbols :: NonEmptyArray SymbolChar
symbols =
  let
    head = singleton '!' -- 0x21

    tail =
      catMaybes $ fromCharCode
        <$> concat
            [ 0x22 .. 0x2f
            , 0x3a .. 0x40
            , 0x5b .. 0x60
            , 0x7b .. 0x7e
            ]
  in
    SymbolChar <$> (head `appendArray` tail)

genSymbols :: forall m. MonadGen m => m SymbolChar
genSymbols = elements symbols

toSymbolChar :: Char -> Maybe SymbolChar
toSymbolChar c =
  let
    s = SymbolChar c
  in
    if s `elem` symbols then
      Just s
    else
      Nothing

toNonEmptySymbolCharArrray :: String -> Either String (NonEmptyArray SymbolChar)
toNonEmptySymbolCharArrray = fromString >=> traverse toSymbolCharE
  where
  fromString =
    Str.fromString
      >>> (map toNonEmptyCharArray)
      >>> note "symbol charset should be non empty"

  toSymbolCharE c = note ("Invalid Symbol:" <> show c) $ toSymbolChar c

module Data.Char.Symbols where

import Prelude
import Control.Monad.Gen (elements)
import Data.Array (concat, (..), catMaybes)
import Data.Array.NonEmpty (NonEmptyArray, appendArray, singleton, elem)
import Data.Char (fromCharCode)
import Data.Either (Either, note)
import Data.Traversable (traverse)
import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype)
import Data.String.NonEmpty (fromString) as Str
import Data.String.NonEmpty.CodeUnits (toNonEmptyCharArray)
import Test.QuickCheck (class Arbitrary)

newtype SymbolChar
  = SymbolChar Char

derive instance eqSymbolChar :: Eq SymbolChar

derive instance newtypePasswd :: Newtype SymbolChar _

derive newtype instance showPasswd :: Show SymbolChar

instance arbitarySymbolChar :: Arbitrary SymbolChar where
  arbitrary = elements symbols

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

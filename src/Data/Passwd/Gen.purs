module Data.Passwd.Gen (genPasswd) where

import Prelude
import Control.Monad.Gen (class MonadGen, oneOf)
import Data.Array.NonEmpty (NonEmptyArray, (:), appendArray, singleton, toArray)
import Data.Char.Gen (genDigitChar, genAlphaLowercase, genAlphaUppercase)
import Data.Char.Symbols.Gen (genSymbolChar)
import Data.Count (fromCount)
import Data.Foldable (sum)
import Data.Length (fromLength)
import Data.Passwd (Passwd(Passwd))
import Data.Policy (Policy)
import Data.String.CodeUnits (fromCharArray)
import Data.Traversable (traverse)
import Data.Tuple (Tuple(..), fst, snd, uncurry)
import Test.QuickCheck.Gen (Gen, vectorOf, shuffle)

genPasswd :: Policy -> Gen Passwd
genPasswd policy@{ length } =
  let
    required = toCharTypeArray policy

    requiredLength = sum $ fst <$> required

    remain = max 0 $ (fromLength length) - requiredLength

    genAll = oneOf $ snd <$> required

    genWhole = (Tuple remain genAll) : required
  in
    do
      charsets <- traverse (uncurry vectorOf) genWhole
      passcode <- shuffle (join $ toArray charsets)
      pure (Passwd $ fromCharArray passcode)

toCharTypeArray :: forall m. MonadGen m => Policy -> NonEmptyArray (Tuple Int (m Char))
toCharTypeArray p =
  singleton (Tuple (fromCount p.digitNum) genDigitChar)
    `appendArray`
      [ Tuple (fromCount p.capitalNum) genAlphaUppercase
      , Tuple (fromCount p.lowercaseNum) genAlphaLowercase
      , Tuple (fromCount p.symbolNum) genSymbolChar
      ]

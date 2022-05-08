module Data.Passwd.Gen (genPasswd) where

import Prelude
import Control.Monad.Gen (class MonadGen, elements, oneOf)
import Data.Array (catMaybes)
import Data.Array.NonEmpty (NonEmptyArray, toArray, fromArray, (:))
import Data.Char.Gen (genDigitChar, genAlphaLowercase, genAlphaUppercase)
import Data.Count (fromCount)
import Data.Either (Either, note)
import Data.Foldable (sum)
import Data.Length (fromLength)
import Data.Newtype (unwrap)
import Data.Passwd (Passwd(Passwd))
import Data.Policy (Policy)
import Data.String.CodeUnits (fromCharArray)
import Data.Traversable (traverse)
import Data.Tuple (Tuple(..), fst, snd, uncurry)
import Test.QuickCheck.Gen (Gen, vectorOf, shuffle)

genPasswd :: Policy -> Either String (Gen Passwd)
genPasswd policy@{ length } = ado
  required <- toCharTypeArray policy
  let
    requiredLength = sum $ fst <$> required

    remain = max 0 $ (fromLength length) - requiredLength

    genAll = oneOf $ snd <$> required

    genWhole = (Tuple remain genAll) : required
  in fromCharGenArray genWhole
  where
  fromCharGenArray gs = do
    charsets <- traverse (uncurry vectorOf) gs
    passcode <- shuffle (join $ toArray charsets)
    pure (Passwd $ fromCharArray passcode)

toCharTypeArray :: forall m. MonadGen m => Policy -> Either String (NonEmptyArray (Tuple Int (m Char)))
toCharTypeArray p =
  catMaybes
    [ p.digitNum <#> \c -> Tuple (fromCount c) genDigitChar
    , p.capitalNum <#> \c -> Tuple (fromCount c) genAlphaUppercase
    , p.lowercaseNum <#> \c -> Tuple (fromCount c) genAlphaLowercase
    , p.symbolNum <#> \{ count, charset } -> Tuple (fromCount count) (elements $ unwrap <$> charset)
    ]
    # fromArray
    # note "At least one character type must be included."

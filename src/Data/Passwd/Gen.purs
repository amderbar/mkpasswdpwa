module Data.Passwd.Gen (genPasswd) where

import Prelude

import Control.Monad.Gen (oneOf)
import Data.Array.NonEmpty (toArray, (:))
import Data.Char.GenSource (arbitraryIn)
import Data.Count (fromCount)
import Data.Foldable (sum)
import Data.Length (fromLength)
import Data.Passwd (Passwd(Passwd))
import Data.Policy (Policy, CharTypeConf)
import Data.String.CodeUnits (fromCharArray)
import Data.Traversable (traverse)
import Data.Tuple (Tuple(..), fst, snd, uncurry)
import Test.QuickCheck.Gen (Gen, vectorOf, shuffle)

genPasswd :: Policy -> Gen Passwd
genPasswd { length, required } =
  let
    requiredGen = tupleCharType <$> required

    minLen = sum $ fst <$> requiredGen

    remain = max 0 $ (fromLength length) - minLen

    genAll = oneOf $ snd <$> requiredGen
  in
    fromCharGenArray $ (Tuple remain genAll) : requiredGen
  where
  fromCharGenArray gs = do
    charsets <- traverse (uncurry vectorOf) gs
    passcode <- shuffle (join $ toArray charsets)
    pure (Passwd $ fromCharArray passcode)

  tupleCharType :: CharTypeConf -> Tuple Int (Gen Char)
  tupleCharType {count, genSrc} =
    let cnt = fromCount count
        gen = arbitraryIn genSrc
    in Tuple cnt gen

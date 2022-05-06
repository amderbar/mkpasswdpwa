module Mkpasswd where

import Prelude (join, ($), (-), (<$>), max, pure, bind)
import Control.Monad.Gen (oneOf)
import Data.Array ((:))
import Data.Array.NonEmpty (toArray)
import Data.Foldable (sum)
import Data.PasswdPolicy (PasswdPolicy)
import Data.String.CodeUnits (fromCharArray)
import Data.Traversable (traverse)
import Data.Tuple (Tuple(..), fst, snd, uncurry)
import Effect (Effect)
import Test.QuickCheck.Gen (Gen, vectorOf, shuffle, randomSampleOne)

mkpasswd :: PasswdPolicy Gen -> Effect String
mkpasswd policy = randomSampleOne (genPassword policy)

genPassword :: PasswdPolicy Gen -> Gen String
genPassword { length, required } =
  let
    requiredLength = sum $ fst <$> required

    remain = max 0 (length - requiredLength)

    genAll = oneOf $ snd <$> required
  in
    do
      charsets <- traverse (uncurry vectorOf) $ (Tuple remain genAll) : toArray required
      passcode <- shuffle (join charsets)
      pure (fromCharArray passcode)

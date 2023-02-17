module Data.Count
  ( Count
  , fromCount
  , toCount
  , toCountE
  ) where

import Prelude

import Control.Monad.Error.Class (liftEither)
import Control.Monad.Gen (chooseInt)
import Data.Either (Either, note)
import Data.Int.Interval (class IntInterval)
import Data.Maybe (Maybe(..))
import Test.QuickCheck.Arbitrary (class Arbitrary)

newtype Count
  = Count Int

toCount :: Int -> Maybe Count
toCount i =
  let
    l = Count i
  in
    if between bottom top l then
      Just l
    else
      Nothing

toCountE :: forall e. (Int -> Int -> e) -> Int -> Either e Count
toCountE f =
  let
    err = f (fromCount bottom) (fromCount top)
  in
    toCount >>> note err

fromCount :: Count -> Int
fromCount (Count i) = i

derive instance eqCount :: Eq Count

derive instance ordCount :: Ord Count

instance boundCount :: Bounded Count where
  top = Count 100
  bottom = Count 0

instance showCount :: Show Count where
  show (Count l) = "(Count " <> show l <> ")"

instance arbitaryCount :: Arbitrary Count where
  arbitrary =
    let
      Count b = bottom

      Count t = top
    in
      Count <$> (chooseInt b t)

instance intIntervalLngth :: IntInterval Count where
  toInt = fromCount
  fromInt e = liftEither <<< toCountE e

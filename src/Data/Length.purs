module Data.Length
  ( Length
  , fromLength
  , toLength
  )
  where

import Prelude
import Control.Monad.Gen (chooseInt)
import Data.Maybe (Maybe(..))
import Test.QuickCheck.Arbitrary (class Arbitrary)

newtype Length
  = Length Int

toLength :: Int -> Maybe Length
toLength i =
  let
    l = Length i
  in
    if between bottom top l then
      Just l
    else
      Nothing

fromLength :: Length -> Int
fromLength (Length i) = i

derive instance eqLength :: Eq Length

derive instance ordLength :: Ord Length

instance boundLength :: Bounded Length where
  top = Length 100
  bottom = Length 1

instance showLength :: Show Length where
  show (Length l) = "(Length " <> show l <> ")"

instance arbitaryLength :: Arbitrary Length where
  arbitrary =
    let
      Length b = bottom

      Length t = top
    in
      Length <$> (chooseInt b t)

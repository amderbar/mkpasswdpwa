module Data.Int.Interval where

import Prelude

import Control.Monad.Error.Class (class MonadError)
import Data.Int as Int

class Bounded a <= IntInterval a where
  toInt :: a -> Int
  fromInt :: forall e m. MonadError e m => (Int -> Int -> e) -> Int -> m a

instance intIntervalInt :: IntInterval Int where
  toInt = identity
  fromInt _ = pure

toNumber :: forall a. IntInterval a => a -> Number
toNumber = Int.toNumber <<< toInt

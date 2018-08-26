module Mkpasswd.Data.Array where

import Prelude
import Data.Array              as Arr
import Data.Maybe                 (fromMaybe)

modifyAt :: forall a. Int -> (a -> a) -> Array a -> Array a
modifyAt i f a = fromMaybe a $ Arr.modifyAt i f a

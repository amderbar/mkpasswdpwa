module Mkpasswd.Data.Array where

import Prelude
import Data.Array              as Arr
import Data.Maybe                 (fromMaybe)

modifyAt :: forall a. Int -> (a -> a) -> Array a -> Array a
modifyAt i f a = fromMaybe a $ Arr.modifyAt i f a

updateAt :: forall a. Int -> a -> Array a -> Array a
updateAt i v a = fromMaybe a $ Arr.updateAt i v a

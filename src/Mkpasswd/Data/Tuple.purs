module Mkpasswd.Data.Tuple where

import Prelude
import Data.Tuple                 (Tuple, swap)

updateFst :: forall a b c. b -> Tuple a c -> Tuple b c
updateFst n = modifyFst (const n)

updateSnd :: forall a b c. b -> Tuple c a -> Tuple c b
updateSnd n = modifySnd (const n)

modifyFst :: forall a b c. (a -> b) -> Tuple a c -> Tuple b c
modifyFst f = swap >>> modifySnd f >>> swap

modifySnd :: forall a b c. (a -> b) -> Tuple c a -> Tuple c b
modifySnd = map

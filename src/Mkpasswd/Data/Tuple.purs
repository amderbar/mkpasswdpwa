module Mkpasswd.Data.Tuple where

import Prelude
import Control.Biapplicative      ( bipure )
import Control.Biapply            ( (<<*>>) )
import Data.Semigroup             ( class Semigroup )
import Data.Semiring              ( class Semiring )
import Data.Tuple                 (Tuple, swap)

addappend :: forall r g
           . Semiring r
          => Semigroup g
          => Tuple r g -> Tuple r g -> Tuple r g
addappend a b = bipure (+) (<>) <<*>> a <<*>> b

infixl 5 addappend as <+>

updateFst :: forall a b c. b -> Tuple a c -> Tuple b c
updateFst n = modifyFst (const n)

updateSnd :: forall a b c. b -> Tuple c a -> Tuple c b
updateSnd n = modifySnd (const n)

modifyFst :: forall a b c. (a -> b) -> Tuple a c -> Tuple b c
modifyFst f = swap >>> modifySnd f >>> swap

modifySnd :: forall a b c. (a -> b) -> Tuple c a -> Tuple c b
modifySnd = map

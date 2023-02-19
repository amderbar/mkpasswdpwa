module Data.FunctorB where

import Prelude

class FunctorB (a :: (Type -> Type) -> Type) where
  bmap :: forall f g. (f ~> g) -> a f -> a g

infixl 4 bmap as <~>

module Data.Switch where

import Prelude

import Data.Either (Either(..))
import Data.Generic.Rep (class Generic)
import Data.Maybe (Maybe(..))
import Data.Show.Generic (genericShow)
import Data.Traversable (class Foldable, class Traversable, sequence)

data State = Off | On

derive instance eqState :: Eq State

derive instance ordState :: Ord State

derive instance genericState :: Generic State _

instance showState :: Show State where
  show = genericShow

toState :: Boolean -> State
toState b = if b then On else Off

data Switch a = Switch State a

derive instance eqSwitch :: Eq a => Eq (Switch a)

derive instance ordSwitch :: Ord a => Ord (Switch a)

derive instance genericSwitch :: Generic (Switch a) _

instance showSwitch :: Show a => Show (Switch a) where
  show = genericShow

derive instance functorSwitch :: Functor Switch

instance foldableSwitch :: Foldable Switch where
  foldr f z (Switch _ x) = x `f` z
  foldl f z (Switch _ x) = z `f` x
  foldMap f (Switch _ x) = f x

instance traversableSwitch :: Traversable Switch where
  traverse f = sequence <<< map f
  sequence (Switch s ma) = Switch s <$> ma

toSwitch :: forall a. Boolean -> a -> Switch a
toSwitch s a = Switch (toState s) a

state :: forall a. Switch a -> State
state (Switch s _) = s

label :: forall a. Switch a -> a
label (Switch _ a) = a

isOn :: forall a. Switch a -> Boolean
isOn s = state s == On

on :: forall a. Switch a -> Switch a
on (Switch _ a) = Switch On a

off :: forall a. Switch a -> Switch a
off (Switch _ a) = Switch Off a

toggle :: forall a. Switch a -> Switch a
toggle s = if isOn s then off s else on s

toMaybe :: Switch ~> Maybe
toMaybe s = if isOn s then Just (label s) else Nothing

fromEither :: forall a. Either a a -> Switch a
fromEither = case _ of
  Right a -> Switch On a
  Left a -> Switch Off a

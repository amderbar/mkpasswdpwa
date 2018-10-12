module Mkpasswd.Data.Switch where

import Prelude
import Data.Maybe                 (Maybe(..))
import Data.Tuple                 (Tuple(..), fst, snd)
import Mkpasswd.Data.Tuple        (updateFst)

newtype Switch a = Switch (Tuple Boolean a)

data State = On | Off

toSwitch :: forall a. State -> a -> Switch a
toSwitch On  a = Switch (Tuple true  a)
toSwitch Off a = Switch (Tuple false a)

state :: forall a. Switch a -> State
state s = if isOn s then On else Off

label :: forall a. Switch a -> a
label (Switch t) = snd t

isOn :: forall a. Switch a -> Boolean
isOn (Switch t) = fst t

on :: forall a. Switch a -> Switch a
on (Switch t) = Switch (updateFst true t)

off :: forall a. Switch a -> Switch a
off (Switch t) = Switch (updateFst false t)

toggle :: forall a. Switch a -> Switch a
toggle s = if isOn s then off s else on s

toMaybe :: forall a. Switch a -> Maybe a
toMaybe s = if isOn s then Just (label s) else Nothing

derive instance eqSwitch  :: Eq  a => Eq  (Switch a)
derive instance ordSwitch :: Ord a => Ord (Switch a)

instance showSwitch :: Show a => Show (Switch a) where
    show (Switch (Tuple true  a)) = "on ("  <> show a <> ")"
    show (Switch (Tuple false a)) = "off (" <> show a <> ")"

derive newtype instance functorSwitch :: Functor Switch

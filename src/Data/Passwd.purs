module Data.Passwd where

import Prelude
import Data.Generic.Rep (class Generic)
import Data.Newtype (class Newtype)
import Data.Show.Generic (genericShow)

newtype Passwd = Passwd String

derive instance eqPasswd :: Eq Passwd

derive instance genericPasswd :: Generic Passwd _

instance showPasswd :: Show Passwd where
  show = genericShow

derive instance newtypePasswd :: Newtype Passwd _

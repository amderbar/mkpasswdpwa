module Mkpasswd.Data.Validation where

import Prelude
import Data.Generic.Rep           (class Generic)
import Data.Generic.Rep.Show      (genericShow)
import Data.Validation.Semigroup  (V, invalid)

data ErrorCode
    = OutOfRange
    | ValueMissing
    | EmptyCharSet
    | TooShort
    | Unknown

derive instance genericErrorReason :: Generic ErrorCode _
instance showErrorReason :: Show ErrorCode where
    show = genericShow

chk :: forall e v. e -> (v -> Boolean) -> v -> V (Array e) Unit
chk e r v = if r v
    then pure unit
    else invalid [e]

maxRule :: Int -> Int -> Boolean
maxRule = (>=)

minRule :: Int -> Int -> Boolean
minRule = (<=)

rangeRule :: Int ->  Int -> Int -> Boolean
rangeRule = between

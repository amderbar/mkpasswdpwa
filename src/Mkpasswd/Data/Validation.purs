module Mkpasswd.Data.Validation where

import Prelude
import Data.Generic.Rep           (class Generic)
import Data.Generic.Rep.Show      (genericShow)
import Data.Array               as Arr
import Data.Either                (Either(..), note)
import Data.Int                   (fromString)
import Data.String              as Str

data ErrorCode
    = OutOfRange
    | ValueMissing
    | EmptyCharSet
    | TooShort
    | TypeMismatch
    | Invalid
    | Unknown

derive instance genericErrorReason :: Generic ErrorCode _
instance showErrorReason :: Show ErrorCode where
    show = genericShow

int :: forall e. e -> String -> Either (Array e) Int
int e = note [e] <<< fromString

chk :: forall e v. e -> (v -> Boolean) -> v -> Either (Array e) v
chk e r v = if r v
    then pure v
    else Left [e]

requiredRule :: String -> Boolean
requiredRule = not Str.null

nonEmptyRule :: forall a. Array a -> Boolean
nonEmptyRule = not Arr.null

maxRule :: Int -> Int -> Boolean
maxRule = (>=)

minRule :: Int -> Int -> Boolean
minRule = (<=)

rangeRule :: Int ->  Int -> Int -> Boolean
rangeRule = between

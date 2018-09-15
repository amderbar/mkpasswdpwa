module Mkpasswd.Data.PasswdPolicy.Validation where

import Prelude
import Mkpasswd.Data.PasswdPolicy
import Control.Biapplicative      (bipure)
import Data.Array                 (null)
import Data.Foldable              (all)
import Data.Generic.Rep           (class Generic)
import Data.Generic.Rep.Show      (genericShow)
import Data.Traversable           (traverse)
import Data.Tuple                 (Tuple(..), uncurry)
import Data.Validation.Semigroup  (V, invalid)

data ErrorCode
    = OutOfRange
    | EmptyCharSet
    | TooShort

derive instance genericErrorReason :: Generic ErrorCode _
instance showErrorReason :: Show ErrorCode where
    show = genericShow

validate :: Int -> Array PasswdPolicy -> V (Array ErrorCode) (Tuple Int (Array PasswdPolicy))
validate l p
    =  chk OutOfRange (rangeRule 0 100) l
    *> chk EmptyCharSet nonEmptyRule p
    *> traverse requiredMinNumLengthChk (requiredMinNum <$> p)
    *> traverse charSetNonEmptyChk (charSet <$> p)
    *> chk TooShort (uncurry enoughLengthRule) (bipure l p)
    *> pure (bipure l p)
    where requiredMinNumLengthChk n = chk OutOfRange (rangeRule 0 100) n
          charSetNonEmptyChk s = chk EmptyCharSet nonEmptyRule s

type Rule = forall a e.
    { rule :: a -> Boolean
    , err  :: e
    }

chk e r v = if r v
    then pure unit
    else invalid [e]

rangeRule l u x = l <= x && x <= u

nonEmptyRule = not null

enoughLengthRule l p = minLength p <= l

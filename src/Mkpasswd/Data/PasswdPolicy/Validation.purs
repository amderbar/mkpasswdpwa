module Mkpasswd.Data.PasswdPolicy.Validation where

import Prelude
import Control.Biapplicative      (bipure)
import Data.Array                 (null)
import Data.Traversable           (traverse)
import Data.Tuple                 (Tuple, uncurry)
import Data.Validation.Semigroup  (V)

import Mkpasswd.Data.PasswdPolicy (PasswdPolicy, requiredMinLength)
import Mkpasswd.Data.Validation   (ErrorCode(..), chk, rangeRule)

validate :: Int -> Array PasswdPolicy -> V (Array ErrorCode) (Tuple Int (Array PasswdPolicy))
validate l p
    =  chk OutOfRange (rangeRule 0 100) l
    *> chk EmptyCharSet nonEmptyRule p
    *> traverse (chk OutOfRange (rangeRule 0 100)) (_.requiredMinNum <$> p)
    *> traverse (chk EmptyCharSet nonEmptyRule)    (_.charSet <$> p)
    *> chk TooShort (uncurry enoughLengthRule) (bipure l p)
    *> pure (bipure l p)

nonEmptyRule :: forall a. Array a -> Boolean
nonEmptyRule = not null

enoughLengthRule :: Int -> Array PasswdPolicy -> Boolean
enoughLengthRule l p = requiredMinLength p <= l

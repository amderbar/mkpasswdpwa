module Mkpasswd.Data.PasswdPolicy.Validation where

import Prelude
import Mkpasswd.Data.PasswdPolicy
import Mkpasswd.Data.Validation
import Control.Biapplicative      (bipure)
import Data.Array                 (null)
import Data.Traversable           (traverse)
import Data.Tuple                 (Tuple(..), uncurry)
import Data.Validation.Semigroup  (V)

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

nonEmptyRule :: forall a. Array a -> Boolean
nonEmptyRule = not null

enoughLengthRule :: Int -> Array PasswdPolicy -> Boolean
enoughLengthRule l p = minLength p <= l

module Mkpasswd.Data.PasswdPolicy where

import Prelude
import Mkpasswd.Data.Ascii
import Data.Array            ((:), nub)
import Data.Foldable         (foldl)
import Data.Tuple            (Tuple, fst, snd)

type PasswdPolicy =
   { requiredMinNum :: Int
   , charSet        :: Array CharCode
   }

passwdPolicy :: Int -> Array CharCode -> PasswdPolicy
passwdPolicy = { requiredMinNum: _, charSet: _ }

defaultLength :: Int
defaultLength = 9

defaultPolicy :: Array PasswdPolicy
defaultPolicy =
    [ passwdPolicy 2 degits
    , passwdPolicy 2 uppercaseAlphabetics
    , passwdPolicy 2 lowercaseAlphabetics
    , passwdPolicy 1 symbols
    ]

emptyPolicy :: PasswdPolicy
emptyPolicy = passwdPolicy 0 []

combine :: PasswdPolicy -> PasswdPolicy -> PasswdPolicy
combine a p =
    { requiredMinNum: a.requiredMinNum + p.requiredMinNum
    , charSet       : a.charSet <> p.charSet
    }

requiredMinLength :: Array PasswdPolicy -> Int
requiredMinLength p = _.requiredMinNum $ foldl combine emptyPolicy p

normalize :: Int -> Array PasswdPolicy -> Array PasswdPolicy
normalize len polArr =
    let s = rest len $ foldl combine emptyPolicy polArr
     in s : polArr
     where
           rest :: Int -> PasswdPolicy -> PasswdPolicy
           rest l p =
               { requiredMinNum: l - p.requiredMinNum
               , charSet       : nub p.charSet
               }

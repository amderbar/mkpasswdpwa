module Mkpasswd.Data.PasswdPolicy where

import Prelude
import Mkpasswd.Data.Ascii
import Control.Biapplicative ( bipure )
import Control.Biapply       ( (<<*>>) )
import Data.Array            ( (:)
                             , nub
                             )
import Data.Foldable         ( foldl )
import Data.Tuple            ( Tuple, fst, snd )

type RequiredMinNum = Int
type PasswdPolicy = Tuple RequiredMinNum (Array CharCode)

passwdPolicy :: RequiredMinNum -> Array CharCode -> PasswdPolicy
passwdPolicy = bipure

defaultLength :: Int
defaultLength = 9

defaultPolicy :: Array PasswdPolicy
defaultPolicy =
    [ passwdPolicy 2 degits
    , passwdPolicy 2 uppercaseAlphabetics
    , passwdPolicy 2 lowercaseAlphabetics
    , passwdPolicy 1 symbols
    ]

addappend :: PasswdPolicy -> PasswdPolicy -> PasswdPolicy
addappend a b = bipure (+) (<>) <<*>> a <<*>> b

infixl 5 addappend as <+>

sumPolicy :: Array PasswdPolicy -> PasswdPolicy
sumPolicy polArr = foldl (<+>) (passwdPolicy 0 []) polArr

minLength :: Array PasswdPolicy -> RequiredMinNum
minLength p = fst $ sumPolicy p

availableChars :: Array PasswdPolicy -> Array CharCode
availableChars p = nub $ snd $ sumPolicy p

normalize :: Int -> Array PasswdPolicy -> Array PasswdPolicy
normalize len polArr =
    let s = bipure ((-) len) nub <<*>> (sumPolicy polArr)
     in s : polArr

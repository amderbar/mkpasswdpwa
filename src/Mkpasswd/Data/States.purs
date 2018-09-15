module Mkpasswd.Data.States where

import Prelude
import Data.Generic.Rep          (class Generic)
import Data.Generic.Rep.Show     (genericShow)
import Data.String               (length, null)
import Data.Validation.Semigroup (V, invalid)

type FormData =
    { account :: String
    , passwd  :: String
    , note    :: String
    }

initialForm :: FormData
initialForm =
    { account: ""
    , passwd : ""
    , note   : ""
    }

data ErrorCode
    = ValueMissing
    | OutOfRange

derive instance genericErrorReason :: Generic ErrorCode _
instance showErrorReason :: Show ErrorCode where
    show = genericShow

validate f
    =  chk ValueMissing requiredRule f.account
    *> chk ValueMissing requiredRule f.passwd
    *> chk OutOfRange (maxRule 100) (length f.account)
    *> chk OutOfRange (maxRule 100) (length f.passwd)
    *> chk OutOfRange (maxRule 1000) (length f.note)
    *> pure f

chk e r v = if r v
    then pure unit
    else invalid [e]

requiredRule = not null

maxRule u x = x <= u

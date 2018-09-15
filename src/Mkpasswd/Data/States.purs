module Mkpasswd.Data.States where

import Prelude
import Mkpasswd.Data.Validation
import Data.String               (length, null)
import Data.Validation.Semigroup (V)

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

validate f
    =  chk ValueMissing requiredRule f.account
    *> chk ValueMissing requiredRule f.passwd
    *> chk OutOfRange (maxRule 100) (length f.account)
    *> chk OutOfRange (maxRule 100) (length f.passwd)
    *> chk OutOfRange (maxRule 1000) (length f.note)
    *> pure f

requiredRule = not null

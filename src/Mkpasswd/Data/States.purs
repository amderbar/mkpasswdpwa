module Mkpasswd.Data.States where

import Prelude                   (not, pure, (*>))
import Mkpasswd.Data.Validation  (ErrorCode(..), chk, maxRule)
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

validate :: FormData -> V (Array ErrorCode) FormData
validate f
    =  chk ValueMissing requiredRule f.account
    *> chk ValueMissing requiredRule f.passwd
    *> chk OutOfRange (maxRule 100) (length f.account)
    *> chk OutOfRange (maxRule 100) (length f.passwd)
    *> chk OutOfRange (maxRule 1000) (length f.note)
    *> pure f

requiredRule :: String -> Boolean
requiredRule = not null

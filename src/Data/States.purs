module Mkpasswd.Data.States where

type FormData
  = { account :: String
    , passwd :: String
    , note :: String
    }

initialForm :: FormData
initialForm =
  { account: ""
  , passwd: ""
  , note: ""
  }

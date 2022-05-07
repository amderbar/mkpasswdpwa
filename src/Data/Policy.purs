module Data.Policy where

import Data.Count (Count, toCount)
import Data.Length (Length, toLength)
import Data.Maybe (Maybe)

type Policy
  = { length :: Length
    , digitNum :: Maybe Count
    , lowercaseNum :: Maybe Count
    , capitalNum :: Maybe Count
    , symbolNum :: Maybe Count
    }

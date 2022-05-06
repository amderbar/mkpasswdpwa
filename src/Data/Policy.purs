module Data.Policy where

import Data.Length (Length)
import Data.Count (Count)

type Policy
  = { length :: Length
    , digitNum :: Count
    , lowercaseNum :: Count
    , capitalNum :: Count
    , symbolNum :: Count
    }

module Data.PasswdPolicy where

import Data.NonEmpty (NonEmpty)
import Data.Tuple (Tuple)

type PasswdPolicy m
  = { length :: Int
    , required :: NonEmpty Array (Tuple Int (m Char))
    }

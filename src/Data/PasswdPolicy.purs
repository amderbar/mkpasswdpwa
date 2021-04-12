module Data.PasswdPolicy where

import Data.Array.NonEmpty (NonEmptyArray)
import Data.Tuple (Tuple)

type PasswdPolicy m
  = { length :: Int
    , required :: NonEmptyArray (Tuple Int (m Char))
    }

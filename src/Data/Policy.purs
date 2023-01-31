module Data.Policy where

import Prelude

import Data.Array.NonEmpty (NonEmptyArray, fromArray)
import Data.Char.GenSource (GenSrcExists, digits, lowercases, mkGenSource, uppercases)
import Data.Char.Subset (symbols)
import Data.Count (Count, toCount)
import Data.Length (Length, toLength)
import Data.Maybe (Maybe, fromJust)
import Partial.Unsafe (unsafePartial)

type CharTypeConf
  = { count :: Count
    , genSrc :: GenSrcExists
    }

type Policy
  = { length :: Length
    , required :: NonEmptyArray CharTypeConf
    }

mkPolicy :: Length -> Array CharTypeConf -> Maybe Policy
mkPolicy length confs = { length, required: _ } <$> fromArray confs

defaultPolicy :: Policy
defaultPolicy = unsafePartial $ fromJust $ do
  len <- toLength 9
  digitsCnt <- toCount 2
  uppercasesCnt <- toCount 2
  lowercasesCnt <- toCount 2
  symbolsCnt <- toCount 1
  mkPolicy len
    [ { count: digitsCnt, genSrc: digits }
    , { count: uppercasesCnt, genSrc: uppercases }
    , { count: lowercasesCnt, genSrc: lowercases }
    , { count: symbolsCnt, genSrc: mkGenSource symbols }
    ]

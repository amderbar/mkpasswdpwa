module Data.Policy where

import Data.Array.NonEmpty (NonEmptyArray, fromArray, toArray)
import Data.Char.Symbols (SymbolChar, symbols)
import Data.Count (Count, toCount)
import Data.Length (Length, toLength)
import Data.Maybe (Maybe)

type CharTypeConf char
  = { count :: Count
    , charset :: NonEmptyArray char
    }

type Policy
  = { length :: Length
    , digitNum :: Maybe Count
    , lowercaseNum :: Maybe Count
    , capitalNum :: Maybe Count
    , symbolNum :: Maybe (CharTypeConf SymbolChar)
    }

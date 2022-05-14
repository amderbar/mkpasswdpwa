module Data.Policy where

import Data.Array.NonEmpty (NonEmptyArray)
import Data.Char.Symbols (SymbolChar)
import Data.Count (Count)
import Data.Length (Length)
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

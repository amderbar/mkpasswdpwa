module Data.Policy where

import Prelude

import Data.Array.NonEmpty (NonEmptyArray, fromArray)
import Data.Char.Gen (genAlphaLowercase, genAlphaUppercase, genDigitChar)
import Data.Char.Subset (Hiragana, SymbolChar, symbols)
import Data.Count (Count, toCount)
import Data.GenSource (class GenSource, arbitraryIn, members)
import Data.Length (Length, toLength)
import Data.Maybe (Maybe, fromJust)
import Data.String.NonEmpty (NonEmptyString, nes)
import Data.String.NonEmpty.CodeUnits (toNonEmptyCharArray)
import Partial.Unsafe (unsafePartial)
import Type.Proxy (Proxy(..))

type GenSrcName = String

data CharGenSrc
  = Digits
  | UppercaseAlphabets
  | LowercaseAlphabets
  | Symbols (NonEmptyArray SymbolChar)
  | Hiraganas (NonEmptyArray Hiragana)
  | AnyChars GenSrcName NonEmptyString

instance genSourceSubsetChar :: GenSource CharGenSrc Char where
  members = case _ of
    Digits -> toNonEmptyCharArray $ nes (Proxy :: Proxy "1234567890")
    UppercaseAlphabets -> toNonEmptyCharArray $ nes (Proxy :: Proxy "ABCDEFGHIJKLMNOPQRSTUVWXYZ")
    LowercaseAlphabets -> toNonEmptyCharArray $ nes (Proxy :: Proxy "abcdefghijklmnopqrstuvwxyz")
    Symbols cs -> members cs
    Hiraganas cs -> members cs
    AnyChars _ cs -> members cs

  arbitraryIn = case _ of
    Digits -> genDigitChar
    UppercaseAlphabets -> genAlphaUppercase
    LowercaseAlphabets -> genAlphaLowercase
    Symbols cs -> arbitraryIn cs
    Hiraganas cs -> arbitraryIn cs
    AnyChars _ cs -> arbitraryIn cs

type CharTypeConf
  = { count :: Count
    , genSrc :: CharGenSrc
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
    [ { count: digitsCnt, genSrc: Digits }
    , { count: uppercasesCnt, genSrc: UppercaseAlphabets }
    , { count: lowercasesCnt, genSrc: LowercaseAlphabets }
    , { count: symbolsCnt, genSrc: Symbols symbols }
    ]

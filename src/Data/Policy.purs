module Data.Policy where

import Prelude

import Data.Array.NonEmpty (NonEmptyArray, fromArray)
import Data.Char.Gen (genAlphaLowercase, genAlphaUppercase, genDigitChar)
import Data.Char.Subset (Hiragana, SymbolChar, hiragana, symbols, toChar)
import Data.Count (Count, toCount)
import Data.FunctorB (class FunctorB)
import Data.GenSource (class GenSource, arbitraryIn, members)
import Data.Length (Length, toLength)
import Data.Maybe (fromJust)
import Data.String.NonEmpty (nes)
import Partial.Unsafe (unsafePartial)
import Type.Proxy (Proxy(..))

data CharGenSrc wrap
  = Digits
  | UppercaseAlphabets
  | LowercaseAlphabets
  | Symbols (wrap SymbolChar)
  | Hiraganas (wrap Hiragana)
  | AnyChars (wrap Char)

instance genSourceSubsetChar :: GenSource (CharGenSrc NonEmptyArray) Char where
  members = case _ of
    Digits -> members $ nes (Proxy :: Proxy "1234567890")
    UppercaseAlphabets -> members $ nes (Proxy :: Proxy "ABCDEFGHIJKLMNOPQRSTUVWXYZ")
    LowercaseAlphabets -> members $ nes (Proxy :: Proxy "abcdefghijklmnopqrstuvwxyz")
    Symbols cs -> members (toChar <$> cs)
    Hiraganas cs -> members (toChar <$> cs)
    AnyChars cs -> members cs

  arbitraryIn = case _ of
    Digits -> genDigitChar
    UppercaseAlphabets -> genAlphaUppercase
    LowercaseAlphabets -> genAlphaLowercase
    Symbols cs -> arbitraryIn (toChar <$> cs)
    Hiraganas cs -> arbitraryIn (toChar <$> cs)
    AnyChars cs -> arbitraryIn cs

instance functorBGenSource :: FunctorB CharGenSrc where
  bmap nt = case _ of
    Digits -> Digits
    UppercaseAlphabets -> UppercaseAlphabets
    LowercaseAlphabets -> LowercaseAlphabets
    Symbols cs -> Symbols (nt cs)
    Hiraganas cs -> Hiraganas (nt cs)
    AnyChars cs -> AnyChars (nt cs)

defaultSymbols ∷ CharGenSrc NonEmptyArray
defaultSymbols = Symbols symbols

defaultHiragana ∷ CharGenSrc NonEmptyArray
defaultHiragana = Hiraganas hiragana

type CharTypeConf =
  { count :: Count
  , genSrc :: CharGenSrc NonEmptyArray
  }

type Policy =
  { length :: Length
  , required :: NonEmptyArray CharTypeConf
  }

defaultPolicy :: Policy
defaultPolicy =
  unsafePartial $ fromJust
    $ do
        len <- toLength 9
        digitsCnt <- toCount 2
        uppercasesCnt <- toCount 2
        lowercasesCnt <- toCount 2
        symbolsCnt <- toCount 1
        req <- fromArray
          [ { count: digitsCnt, genSrc: Digits }
          , { count: uppercasesCnt, genSrc: UppercaseAlphabets }
          , { count: lowercasesCnt, genSrc: LowercaseAlphabets }
          , { count: symbolsCnt, genSrc: defaultSymbols }
          ]
        pure { length: len, required: req }

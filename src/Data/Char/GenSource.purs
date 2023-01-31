module Data.Char.GenSource
  ( Digits(..)
  , GenSrcExists
  , LowercaseAlphabets(..)
  , UppercaseAlphabets(..)
  , arbitraryIn
  , class GenSource
  , digits
  , fromString
  , lowercases
  , members
  , mkGenSource
  , uppercases
  )
  where

import Prelude

import Control.Monad.Gen (class MonadGen, elements)
import Data.Array.NonEmpty (NonEmptyArray)
import Data.Char.Gen (genAlphaLowercase, genAlphaUppercase, genDigitChar)
import Data.Char.Subset (class SubsetChar, toChar)
import Data.Exists (Exists, mkExists, runExists)
import Data.Maybe (Maybe)
import Data.String.NonEmpty (NonEmptyString, nes)
import Data.String.NonEmpty (fromString) as NES
import Data.String.NonEmpty.CodeUnits (toNonEmptyCharArray)
import Type.Proxy (Proxy(..))

class GenSource src where
  members :: src -> NonEmptyArray Char
  arbitraryIn :: forall m. MonadGen m => src -> m Char

instance genSourceDigits :: GenSource Digits where
  members Digits = toNonEmptyCharArray $ nes (Proxy :: Proxy "1234567890")
  arbitraryIn Digits = genDigitChar

else instance genSourceUppercaseAlphabets :: GenSource UppercaseAlphabets where
  members UppercaseAlphabets = toNonEmptyCharArray $ nes (Proxy :: Proxy "ABCDEFGHIJKLMNOPQRSTUVWXYZ")
  arbitraryIn UppercaseAlphabets = genAlphaUppercase

else instance genSourceLowercaseAlphabets :: GenSource LowercaseAlphabets where
  members LowercaseAlphabets = toNonEmptyCharArray $ nes (Proxy :: Proxy "abcdefghijklmnopqrstuvwxyz")
  arbitraryIn LowercaseAlphabets = genAlphaLowercase

else instance genSourceNonEmptyArray :: SubsetChar c => GenSource (NonEmptyArray c) where
  members charset = toChar <$> charset
  arbitraryIn = elements <<< members

else instance genSourceNonEmptyString :: GenSource NonEmptyString where
  members = toNonEmptyCharArray
  arbitraryIn = arbitraryIn <<< members

else instance genSourceExists :: GenSource GenSrcExists where
  members (GenSrcExists src) = src # runExists \(GS {  members: mems, charset }) -> mems charset
  arbitraryIn (GenSrcExists src) = src # runExists \(GS { arbitraryIn: arb, charset }) -> arb charset

data Digits = Digits

data UppercaseAlphabets = UppercaseAlphabets

data LowercaseAlphabets = LowercaseAlphabets

newtype GS src = GS
  { charset :: src
  , members :: src -> NonEmptyArray Char
  , arbitraryIn :: forall m. MonadGen m => src -> m Char
  }

newtype GenSrcExists = GenSrcExists (Exists GS)

mkGenSource :: forall src. GenSource src => src -> GenSrcExists
mkGenSource = GenSrcExists <<< mkExists <<< GS <<< { charset: _, members, arbitraryIn }

fromString :: String -> Maybe GenSrcExists
fromString = map mkGenSource <<< NES.fromString

digits :: GenSrcExists
digits = mkGenSource Digits

uppercases :: GenSrcExists
uppercases = mkGenSource UppercaseAlphabets

lowercases :: GenSrcExists
lowercases = mkGenSource LowercaseAlphabets

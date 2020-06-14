module Mkpasswd where

import Prelude (join, (-), (<$>), max, pure, bind)
import Data.Array (snoc, head, zip)
import Data.Char.Gen (genDigitChar, genAlphaLowercase, genAlphaUppercase)
import Data.Char.Gen.Symbols (genSymbolChar)
import Data.Char.Gen.Visibles (genAsciiChar')
import Data.Foldable (sum)
import Data.Maybe (Maybe)
import Data.PasswdPolicy (PasswdPolicy)
import Data.String.CodeUnits (fromCharArray)
import Data.Traversable (traverse)
import Data.Tuple (uncurry)
import Effect (Effect)
import Test.QuickCheck.Gen (Gen, vectorOf, shuffle, randomSample')

mkpasswd :: PasswdPolicy -> Effect (Maybe String)
mkpasswd policy = head <$> randomSample' 1 (genPassword policy)

genPassword :: PasswdPolicy -> Gen String
genPassword { length, required } =
  let
    requiredArr =
      [ required.degit
      , required.lower
      , required.upper
      , required.symbol
      ]

    requiredLength = sum requiredArr

    remain = max 0 (length - requiredLength)

    zipGen =
      zip
        (requiredArr `snoc` remain)
        [ genDigitChar
        , genAlphaLowercase
        , genAlphaUppercase
        , genSymbolChar
        , genAsciiChar'
        ]
  in
    do
      charsets <- traverse (uncurry vectorOf) zipGen
      passcode <- shuffle (join charsets)
      pure (fromCharArray passcode)

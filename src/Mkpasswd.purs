module Mkpasswd where

import Prelude
import Control.Bind          ( join )
import Data.Array            ( concat
                             , (..)
                             , length
                             , elem
                             , all
                             , (:)
                             , (!!)
                             , deleteAt
                             , replicate
                             , mapMaybe
                             , catMaybes
                             )
import Data.Char             ( fromCharCode )
import Data.Either           ( Either(..) )
import Data.Foldable         ( sum )
import Data.Maybe            ( Maybe(..), fromMaybe )
import Data.String.CodeUnits ( fromCharArray )
import Data.Traversable      ( sequence )
import Data.Tuple            ( Tuple(..), fst, snd )
import Effect                ( Effect )
import Effect.Random         ( randomInt )

type PasswdPolicy =
    { length    :: Int
    , degit     :: Int
    , uppercase :: Int
    , lowercase :: Int
    , symbol    :: Int
    }

defaultPolicy :: PasswdPolicy
defaultPolicy =
    { length   : 9
    , degit    : 2
    , uppercase: 2
    , lowercase: 2
    , symbol   : 1
    }

arrayFrom :: PasswdPolicy -> Array Int
arrayFrom policy =
    [ policy.length
    , policy.degit
    , policy.uppercase
    , policy.lowercase
    , policy.symbol
    ]

ascii :: Tuple Int Int
ascii = Tuple 0x21 0x7e

degits :: Tuple Int Int
degits = Tuple 0x30 0x39

uppercaseAlphabetics :: Tuple Int Int
uppercaseAlphabetics = Tuple 0x41 0x5a

lowercaseAlphabetics :: Tuple Int Int
lowercaseAlphabetics = Tuple 0x61 0x7a

symbols :: Array Int
symbols = concat [ 0x21 .. 0x2e, 0x3a .. 0x40, 0x5b .. 0x60, 0x7b .. 0x7e ]

mkpasswd :: PasswdPolicy -> Effect (Either String String)
mkpasswd policy = sequence $ do
    valiedPolicy <- validatePolicy policy
    pure $ fromCharCodeArray <$> randomCharCodeArray valiedPolicy
    where
          fromCharCodeArray = fromCharArray <<< mapMaybe fromCharCode
          randomCharCodeArray p =
              join $ shuffle <$> concat <$> sequence
                [ randomCharCodes (asciiNum p) randomAscii
                , randomCharCodes (degitNum p) randomDegit
                , randomCharCodes (uppercaseNum p) randomUpper
                , randomCharCodes (lowercaseNum p) randomLower
                , randomCharCodes (symbolNum p) randomSymbol
                ]

validatePolicy :: PasswdPolicy -> Either String PasswdPolicy
validatePolicy policy =
    pure policy
    >>= validate shouldBeAllPositive "policy should be all positive"
    >>= validate shouldBeLongEnough  "policy should be long enough"
    where
          validate rule errMsg target =
              if rule target
                  then Right target
                  else Left errMsg
          shouldBeAllPositive = arrayFrom >>> (all $ (<) 0)
          shouldBeLongEnough policy =
              let policyTotal  = sum $ arrayFrom policy
                  doubleLength = 2 * policy.length
               in
                  policyTotal < doubleLength

shuffle :: forall a. Array a -> Effect (Array a)
shuffle target = catMaybes <$> (shuffleInternal [] target)
    where
          shuffleInternal acc []  = pure $ acc
          shuffleInternal acc arr =
             let len = length arr
              in do
                 idx <- randomInt 0 (len - 1)
                 let bcc = (arr !! idx) : acc
                 let brr = fromMaybe [] $ deleteAt idx arr
                 shuffleInternal bcc brr

randomCharCodes :: Int -> (Effect Int) -> Effect (Array Int)
randomCharCodes length randomChar = sequence $ replicate length randomChar

asciiNum :: PasswdPolicy -> Int
asciiNum policy = policy.length
    - ( policy.degit
      + policy.uppercase
      + policy.lowercase
      + policy.symbol
      )

degitNum :: PasswdPolicy -> Int
degitNum policy = policy.degit

uppercaseNum :: PasswdPolicy -> Int
uppercaseNum policy = policy.uppercase

lowercaseNum :: PasswdPolicy -> Int
lowercaseNum policy = policy.lowercase

symbolNum :: PasswdPolicy -> Int
symbolNum policy = policy.symbol

randomAscii :: Effect Int
randomAscii =
    let top    = snd ascii
        bottom = fst ascii
     in
        randomInt bottom top

randomDegit :: Effect Int
randomDegit =
    let top    = snd degits
        bottom = fst degits
     in
        randomInt bottom top

randomUpper :: Effect Int
randomUpper =
    let top    = snd uppercaseAlphabetics
        bottom = fst uppercaseAlphabetics
     in
        randomInt bottom top

randomLower :: Effect Int
randomLower =
    let top    = snd lowercaseAlphabetics
        bottom = fst lowercaseAlphabetics
     in
        randomInt bottom top

randomSymbol :: Effect Int
randomSymbol =
    let top    = snd ascii
        bottom = fst ascii
     in do
        chr <- randomInt bottom top
        if elem chr symbols
            then pure chr
            else randomSymbol

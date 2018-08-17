module Mkpasswd where

import Prelude
import Data.Array            ( concat
                             , (..)
                             , null
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
import Data.Generic.Rep      ( class Generic )
import Data.Generic.Rep.Show ( genericShow )
import Data.Maybe            ( Maybe(..), fromMaybe )
import Data.String.CodeUnits ( fromCharArray )
import Data.Traversable      ( sequence )
import Data.Tuple            ( Tuple(..), fst, snd )
import Effect                ( Effect )
import Effect.Random         ( randomInt )

type PasswdPolicy =
    { length    :: Int
    , degit     :: Maybe Int
    , uppercase :: Maybe Int
    , lowercase :: Maybe Int
    , symbol    :: Maybe Int
    }

defaultPolicy :: PasswdPolicy
defaultPolicy =
    { length   : 9
    , degit    : Just 2
    , uppercase: Just 2
    , lowercase: Just 2
    , symbol   : Just 1
    }

policyArray :: PasswdPolicy -> Array Int
policyArray policy = catMaybes
    [ Just policy.length
    , policy.degit
    , policy.uppercase
    , policy.lowercase
    , policy.symbol
    ]

charTypePolicyArray :: PasswdPolicy -> Array Int
charTypePolicyArray policy = catMaybes
    [ policy.degit
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

mkpasswd :: PasswdPolicy -> Effect (Either ErrorReason String)
mkpasswd policy = sequence $ do
    valiedPolicy <- validatePolicy policy
    pure $ fromCharCodeArray <$> randomCharCodeArray valiedPolicy
    where
          fromCharCodeArray = fromCharArray <<< mapMaybe fromCharCode
          randomCharCodeArray p =
              join $ shuffle <$> concat <$> sequence
                [ randomCharCodes (remainNum p) (randomRemain p)
                , randomCharCodes (degitNum p) randomDegit
                , randomCharCodes (uppercaseNum p) randomUpper
                , randomCharCodes (lowercaseNum p) randomLower
                , randomCharCodes (symbolNum p) randomSymbol
                ]

data ErrorReason
    = EmptyChars
    | NoNegative
    | TooShortLength

derive instance genericErrorReason :: Generic ErrorReason _
instance showErrorReason :: Show ErrorReason where
    show = genericShow

validatePolicy :: PasswdPolicy -> Either ErrorReason PasswdPolicy
validatePolicy policy =
    pure policy
    >>= validate shouldNotBeNothingAll EmptyChars
    >>= validate shouldBeAllPositive NoNegative
    >>= validate shouldBeLongEnough  TooShortLength
    where
          validate rule errMsg target =
              if rule target
                  then Right target
                  else Left errMsg
          shouldNotBeNothingAll = charTypePolicyArray >>> null >>> not
          shouldBeAllPositive = policyArray >>> (all $ (<=) 0)
          shouldBeLongEnough p =
                  p.length >= (sum $ charTypePolicyArray p)

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

randomCharCodes :: Maybe Int -> (Effect Int) -> Effect (Array Int)
randomCharCodes length randomChar = sequence $ replicate (fromMaybe 0 length) randomChar

degitNum :: PasswdPolicy -> Maybe Int
degitNum policy = policy.degit

uppercaseNum :: PasswdPolicy -> Maybe Int
uppercaseNum policy = policy.uppercase

lowercaseNum :: PasswdPolicy -> Maybe Int
lowercaseNum policy = policy.lowercase

symbolNum :: PasswdPolicy -> Maybe Int
symbolNum policy = policy.symbol

remainNum :: PasswdPolicy -> Maybe Int
remainNum policy = Just $ policy.length - (sum $ charTypePolicyArray policy)

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

randomCharSelect :: Array Int -> Effect Int
randomCharSelect popArr =
    let top    = snd ascii
        bottom = fst ascii
     in do
        chr <- randomInt bottom top
        if elem chr popArr
            then pure chr
            else randomCharSelect popArr

randomSymbol :: Effect Int
randomSymbol = randomCharSelect symbols

randomRemain :: PasswdPolicy -> Effect Int
randomRemain = randomCharSelect <<< remainChars
    where
          remainChars p = concat $ catMaybes
              [ map (constCharTypeArr degits) p.degit
              , map (constCharTypeArr uppercaseAlphabetics) p.uppercase
              , map (constCharTypeArr lowercaseAlphabetics) p.lowercase
              , map (const symbols) p.symbol
              ]
          constCharTypeArr tpl = const $ (fst tpl) .. (snd tpl)

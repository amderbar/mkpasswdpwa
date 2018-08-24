module Mkpasswd where

import Prelude
import Mkpasswd.Data.Ascii
import Mkpasswd.Data.PasswdPolicy
import Mkpasswd.Effect.Random
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
import Data.Traversable      ( sequence, traverse )
import Data.Tuple            ( Tuple(..), fst, snd, uncurry )
import Effect                ( Effect )
import Effect.Random         ( randomInt )


data ErrorCode
    = EmptyChars
    | NoNegative
    | TooShortLength

derive instance genericErrorReason :: Generic ErrorCode _
instance showErrorReason :: Show ErrorCode where
    show = genericShow

mkpasswd :: Int -> Array PasswdPolicy -> Effect String
mkpasswd len pol =
    let policy = normalize len pol
     in fromCharCodeArray <$> mkpasscode policy
    where
          fromCharCodeArray = fromCharArray <<< mapMaybe fromCharCode
          multiChoice n arr = sequence $ replicate n $ choice arr
          mkpasscode p = catMaybes  <<< join <$> traverse (uncurry multiChoice) p

--validatePolicy :: Int -> PasswdPolicy -> Either ErrorReason (Tuple Int PasswdPolicy)
--validatePolicy length policy =
--    pure $ Tuple length policy
--    >>= validate shouldNotBeNothingAll EmptyChars
--    >>= validate shouldBeAllPositive NoNegative
--    >>= validate shouldBeLongEnough  TooShortLength
--    where
--          validate rule errMsg target =
--              if uncarry rule target
--                  then Right target
--                  else Left errMsg
--          shouldNotBeNothingAll _ p =  >>> null >>> not
--          shouldBeAllPositive = policyArray >>> (all $ (<=) 0)
--          shouldBeLongEnough p =
--                  p.length >= (sum $ charTypePolicyArray p)

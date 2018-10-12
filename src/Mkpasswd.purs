module Mkpasswd where

import Prelude                    (join, ($), (<$>), (<<<), (=<<))
import Data.Char                  (fromCharCode)
import Data.Maybe                 (Maybe)
import Data.String.CodeUnits      (fromCharArray)
import Data.Traversable           (sequence, traverse)
import Data.Tuple                 (Tuple(..), uncurry)
import Data.Unfoldable            (replicateA)
import Effect                     (Effect)

import Mkpasswd.Data.PasswdPolicy (PasswdPolicy, normalize)
import Mkpasswd.Effect.Random     (choice, shuffle)

mkpasswd :: Int -> Array PasswdPolicy -> Effect (Maybe String)
mkpasswd len pol =
    let policy = normalize len pol
     in (fromCharCodeArray =<< _) <$> mkpasscode policy
     where
           fromCharCodeArray :: Array Int -> Maybe String
           fromCharCodeArray a = fromCharArray <$> traverse fromCharCode a

           multiChoice :: forall a. Int -> Array a -> Effect (Array (Maybe a))
           multiChoice n arr = replicateA n $ choice arr

           mkpasscode :: Array PasswdPolicy -> Effect (Maybe (Array Int))
           mkpasscode p =
               (traverse shuffle)
               =<< (sequence <<< join)
               <$> traverse (uncurry multiChoice) (parsePolicy <$> p)

           parsePolicy :: PasswdPolicy -> Tuple Int (Array Int)
           parsePolicy p = Tuple p.requiredMinNum p.charSet

module Test.Main where

import Prelude
import Control.Monad.Gen (class MonadGen)
import Control.Monad.Rec.Class (class MonadRec)
import Data.Array ((..), catMaybes)
import Data.Array.NonEmpty (fromArray)
import Data.Char.Gen (genDigitChar, genAlphaLowercase, genAlphaUppercase)
import Data.Char.Gen.Symbols (genSymbolChar)
import Data.Foldable (for_)
import Data.Maybe (fromMaybe, Maybe(..))
import Data.PasswdPolicy (PasswdPolicy)
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Effect.Class.Console (logShow)
import Mkpasswd (mkpasswd)

main :: Effect Unit
main = do
  for_ (0 .. 99) \_ ->
    let
      conf
        = [ Just (Tuple 2 genDigitChar)
          , Just (Tuple 1 genAlphaUppercase)
          , Just (Tuple 1 genAlphaLowercase)
          , Just (Tuple 1 genSymbolChar)
          ]
    in
      logShow =<< fromMaybe "" <$>
        case mkPolicy 9 conf of
          Just policy -> mkpasswd policy
          Nothing -> pure Nothing
  where
  mkPolicy :: forall m. MonadRec m => MonadGen m => Int -> Array (Maybe (Tuple Int (m Char))) -> Maybe (PasswdPolicy m)
  mkPolicy len conf = { length: len, required: _ } <$> (fromArray <<< catMaybes) conf

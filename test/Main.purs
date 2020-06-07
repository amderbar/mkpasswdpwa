module Test.Main where

import Prelude
import Data.Array ((..))
import Data.Foldable (for_)
import Data.Maybe (fromMaybe)
import Data.PasswdPolicy (passwdPolicy)
import Effect (Effect)
import Effect.Class.Console (log)
import Mkpasswd (mkpasswd)

main :: Effect Unit
main = do
  for_ (0 .. 99) \_ -> log =<< fromMaybe "" <$> mkpasswd (passwdPolicy 10 3 3 3 3)

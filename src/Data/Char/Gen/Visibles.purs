module Data.Char.Gen.Visibles where

import Prelude ((<$>), bottom, top)
import Control.Monad.Gen (class MonadGen, chooseInt)
import Data.Enum (toEnumWithDefaults)

-- | Generates a character in the visible ASCII character set.
genAsciiChar' :: forall m. MonadGen m => m Char
genAsciiChar' = toEnumWithDefaults bottom top <$> chooseInt 33 126

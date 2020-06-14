module Data.Char.Gen.Symbols where

import Prelude (($), (<$>))
import Control.Monad.Gen (class MonadGen, suchThat)
import Control.Monad.Rec.Class (class MonadRec)
import Data.Array (concat, elem, (..), catMaybes)
import Data.Char (fromCharCode)
import Data.Char.Gen.Visibles (genAsciiChar')

genSymbolChar :: forall m. MonadRec m => MonadGen m => m Char
genSymbolChar = genAsciiChar' `suchThat` (_ `elem` symbols)

symbols :: Array Char
symbols =
  catMaybes $ fromCharCode
    <$> concat
        [ 0x21 .. 0x2f
        , 0x3a .. 0x40
        , 0x5b .. 0x60
        , 0x7b .. 0x7e
        ]

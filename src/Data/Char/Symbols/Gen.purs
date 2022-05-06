module Data.Char.Symbols.Gen where

import Control.Monad.Gen (class MonadGen, elements)
import Data.Array (concat, (..), catMaybes)
import Data.Array.NonEmpty (NonEmptyArray, appendArray, singleton)
import Data.Char (fromCharCode)
import Prelude (($), (<$>))

genSymbolChar :: forall m. MonadGen m => m Char
genSymbolChar = elements symbols

symbols :: NonEmptyArray Char
symbols =
  let
    head = singleton '!' -- 0x21

    tail =
      catMaybes $ fromCharCode
        <$> concat
            [ 0x22 .. 0x2f
            , 0x3a .. 0x40
            , 0x5b .. 0x60
            , 0x7b .. 0x7e
            ]
  in
    head `appendArray` tail

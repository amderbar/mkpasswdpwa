module Mkpasswd.Data.Ascii where

import Data.Array            ( concat
                             , (..)
                             )

type CharCode = Int

degits :: Array CharCode
degits = 0x30 .. 0x39

uppercaseAlphabetics :: Array CharCode
uppercaseAlphabetics = 0x41 .. 0x5a

lowercaseAlphabetics :: Array CharCode
lowercaseAlphabetics = 0x61 .. 0x7a

symbols :: Array CharCode
symbols = concat [ 0x21 .. 0x2e
                 , 0x3a .. 0x40
                 , 0x5b .. 0x60
                 , 0x7b .. 0x7e
                 ]

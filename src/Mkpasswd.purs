module Mkpasswd where

import Data.Passwd (Passwd)
import Data.Passwd.Gen (genPasswd)
import Data.Policy (Policy)
import Effect (Effect)
import Test.QuickCheck.Gen (randomSampleOne)

mkpasswd :: Policy -> Effect Passwd
mkpasswd policy = randomSampleOne (genPasswd policy)

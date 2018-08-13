module Mkpasswd.Halogen.Util where

import Prelude
import Halogen.HTML            as HH
import Halogen.HTML.Properties as HP

classes :: forall a b. Array String -> HH.IProp ( "class" :: String | b) a
classes = HP.classes <<< map HH.ClassName

style :: forall a b. String -> HH.IProp b a
style = HP.attr $ HH.AttrName "style"

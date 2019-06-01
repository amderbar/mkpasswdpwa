module Mkpasswd.UI.Element where

import Mkpasswd.Halogen.Util       (classes)
import Halogen.HTML                as HH

----------
-- Layout

container :: forall i p. Array (HH.HTML i p) -> HH.HTML i p
container content =
  HH.section
    [ classes [ "section" ] ]
    [ HH.div
      [ classes [ "container" ] ]
      content
    ]

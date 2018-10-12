module Mkpasswd.UI.Templates where

import Prelude
import Mkpasswd.Halogen.Util       (classes)
import Mkpasswd.UI.Routing         (RouteHash(..), routeHref)
import Effect.Aff                  (Aff)
import Halogen                     as H
import Halogen.HTML                as HH
import Halogen.HTML.Events         as HE
import Halogen.HTML.Properties     as HP

headerNav :: forall a b. HH.HTML a b
headerNav =
      HH.nav
          [ classes [ "border", "flex-none", "flex", "justify-center" ] ]
          [ HH.a
              [ classes [ "flex-auto", "border", "p1", "center" ]
              , HP.href $ routeHref Index
              ]
              [ HH.text "つくる" ]
          , HH.a
              [ classes [ "flex-auto", "border", "p1", "center" ]
              , HP.href $ routeHref List
              ]
              [ HH.text "しまう" ]
          ]

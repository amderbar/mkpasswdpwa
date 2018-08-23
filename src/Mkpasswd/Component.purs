module Mkpasswd.Component where

import Prelude
import Mkpasswd.Halogen.Util       (classes, style)
import Mkpasswd.Component.Mkpasswd as Mk
import Mkpasswd.Routing            (RouteHash(..))
import Data.Const                  (Const)
import Data.Maybe                  (Maybe(..))
import Effect.Aff                  (Aff)
import Effect.Class                (liftEffect)
import Effect.Console              (log)
import Halogen                     as H
import Halogen.Component.ChildPath as HC
import Halogen.Data.Prism          (type (<\/>), type (\/))
import Halogen.HTML                as HH
import Halogen.HTML.Properties     as HP

type ChildQuery = Mk.Query <\/> Const Void
type Slot  = Unit \/ Void

cpMkpasswd :: HC.ChildPath Mk.Query ChildQuery Unit Slot
cpMkpasswd = HC.cp1

type State =
    { route  :: RouteHash
    }

data Query a
    = ChangeHash RouteHash a

ui :: H.Component HH.HTML Query Unit Void Aff
ui =
  H.parentComponent
    { initialState: const initialState
    , render
    , eval
    , receiver : const Nothing
    }
    where
          initialState :: State
          initialState =
              { route  : LinkA
              }

          render :: State -> H.ParentHTML Query ChildQuery Slot Aff
          render state =
              HH.main
                [ style "height: 100%"
                , classes [ "flex" , "flex-column" ]
                ]
                --[ headerNav
                [ HH.slot' cpMkpasswd unit Mk.ui unit absurd
                ]
          headerNav =
                HH.nav
                    [ classes [ "border", "flex-none", "flex", "justify-center" ] ]
                    [ HH.a
                        [ classes [ "flex-auto", "border", "p1", "center" ]
                        , HP.href "#link/a"
                        ]
                        [ HH.text "つくる" ]
                    , HH.a
                        [ classes [ "flex-auto", "border", "p1", "center" ]
                        , HP.href "#link/b"
                        ]
                        [ HH.text "しまう" ]
                    ]

          eval :: Query ~> H.ParentDSL State Query ChildQuery Slot Void Aff
          eval (ChangeHash newHash next) = do
              H.modify_ (_ { route = newHash })
              liftEffect $ log $ show newHash
              pure next

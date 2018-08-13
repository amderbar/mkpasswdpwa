module Mkpasswd.Component where

import Prelude
import Mkpasswd.Halogen.Util       (classes, style)
import Mkpasswd.Component.Mkpasswd as Mk
import Mkpasswd.Routing            (RouteHash(..))
import Data.Maybe                  (Maybe(..))
import Effect.Aff                  (Aff)
import Effect.Class                (liftEffect)
import Effect.Console              (log)
import Halogen                     as H
import Halogen.HTML                as HH
import Halogen.HTML.Properties     as HP

data Slot
    = MkpasswdSlot

derive instance eqSlot :: Eq Slot
derive instance ordSlot :: Ord Slot

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

          render :: State -> H.ParentHTML Query Mk.Query Slot Aff
          render state =
              HH.main
                [ style "height: 100%"
                , classes [ "flex" , "flex-column" ]
                ]
                [ HH.nav
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
                , HH.slot MkpasswdSlot Mk.ui unit absurd
                ]

          eval :: Query ~> H.ParentDSL State Query Mk.Query Slot Void Aff
          eval (ChangeHash newHash next) = do
              H.modify_ (_ { route = newHash })
              liftEffect $ log $ show newHash
              pure next

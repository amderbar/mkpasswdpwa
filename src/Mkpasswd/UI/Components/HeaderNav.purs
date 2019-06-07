module Mkpasswd.UI.Components.HeaderNav where

import Prelude
import Mkpasswd.Halogen.Util       (classes)
import Mkpasswd.UI.Routing         (RouteHash(..), routeHref)
import Data.Maybe                  (Maybe(..))
import Halogen                     as H
import Halogen.HTML                as HH
import Halogen.HTML.Events         as HE
import Halogen.HTML.Properties     as HP

type Input =
    Unit

type Message =
    Void

type State =
    { isMenuActive :: Boolean
    }

data Query a
    = SetMenu Boolean a

component :: forall m. H.Component HH.HTML Query Input Message m
component =
  H.component
    { initialState
    , render
    , eval
    , receiver : const Nothing
    }

initialState :: Input -> State
initialState = const { isMenuActive: false }


render :: State -> H.ComponentHTML Query
render s =
  HH.nav
    [ classes [ "navbar", "is-fixed-top", "is-dark" ]
    , HP.attr (HH.AttrName "role") "navigation"
    , HP.attr (HH.AttrName "aria-label") "main navigation"
    ]
    [ HH.div
      [ classes [ "container" ] ]
      [ HH.div
        [ classes [ "navbar-brand" ] ]
        [ HH.a
          [ classes [ "navbar-item" ]
          , HP.href $ routeHref Index
          ]
          [ HH.strong_ [ HH.text "RandomStr" ] ]
        , HH.a
          [ classes [ "navbar-burger", "burger" ]
          , HP.attr (HH.AttrName "role") "button"
          , HP.attr (HH.AttrName "aria-label") "menu"
          , HP.attr (HH.AttrName "aria-expanded") "button"
          , HE.onClick $ HE.input_ (SetMenu $ not s.isMenuActive)
          ]
          [ HH.span [ HP.attr (HH.AttrName "aria-hidden") "true" ] []
          , HH.span [ HP.attr (HH.AttrName "aria-hidden") "true" ] []
          , HH.span [ HP.attr (HH.AttrName "aria-hidden") "true" ] []
          ]
        ]
      , HH.div
        [ classes [ "navbar-menu", if s.isMenuActive then "is-active" else "" ] ]
        [ HH.div
          [ classes [ "navbar-start" ] ]
          [ HH.a
            [ classes [ "navbar-item" ]
            , HE.onClick $ HE.input_ (SetMenu false)
            , HP.href $ routeHref Index
            ]
            [ HH.text "Generate" ]
          , HH.a
            [ classes [ "navbar-item" ]
            , HE.onClick $ HE.input_ (SetMenu false)
            , HP.href $ routeHref List
            ]
            [ HH.text "Gallery" ]
          ]
        , HH.div
          [ classes [ "navbar-end" ] ]
          [ HH.div
            [ classes [ "navbar-item" ] ]
            [ HH.div
              [ classes [ "buttons" ] ]
              []
            ]
          ]
        ]
      ]
    ]


eval :: forall m. Query ~> H.ComponentDSL State Query Message m
eval (SetMenu a next) = do
    H.modify_ (_ { isMenuActive = a })
    pure next

module Mkpasswd.UI.Components.HeaderNav where

import Prelude
import Mkpasswd.Data.Routing (RouteHash(..), hashStr)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP

type Slot id
  = forall q. H.Slot q Void id

type State
  = { isMenuActive :: Boolean
    }

data Action
  = SetMenu Boolean

component :: forall q i o m. H.Component q i o m
component =
  H.mkComponent
    { initialState
    , render
    , eval: H.mkEval $ H.defaultEval { handleAction = handleAction }
    }

initialState :: forall i. i -> State
initialState _ =
  { isMenuActive: false
  }

render :: forall m. State -> H.ComponentHTML Action () m
render s =
  HH.nav
    [ HP.classes $ HH.ClassName <$> [ "navbar", "is-fixed-top", "is-dark" ]
    , HP.attr (HH.AttrName "role") "navigation"
    , HP.attr (HH.AttrName "aria-label") "main navigation"
    ]
    [ HH.div
        [ HP.classes $ HH.ClassName <$> [ "container" ] ]
        [ HH.div
            [ HP.classes $ HH.ClassName <$> [ "navbar-brand" ] ]
            [ HH.a
                [ HP.classes $ HH.ClassName <$> [ "navbar-item" ]
                , HP.href $ hashStr Index
                ]
                [ HH.strong_ [ HH.text "RandomStr" ] ]
            , HH.a
                [ HP.classes $ HH.ClassName <$> [ "navbar-burger", "burger" ]
                , HP.attr (HH.AttrName "role") "button"
                , HP.attr (HH.AttrName "aria-label") "menu"
                , HP.attr (HH.AttrName "aria-expanded") "button"
                , HE.onClick \_ -> SetMenu $ not s.isMenuActive
                ]
                [ HH.span [ HP.attr (HH.AttrName "aria-hidden") "true" ] []
                , HH.span [ HP.attr (HH.AttrName "aria-hidden") "true" ] []
                , HH.span [ HP.attr (HH.AttrName "aria-hidden") "true" ] []
                ]
            ]
        , HH.div
            [ HP.classes $ HH.ClassName <$> [ "navbar-menu", if s.isMenuActive then "is-active" else "" ] ]
            [ HH.div
                [ HP.classes $ HH.ClassName <$> [ "navbar-start" ] ]
                [ HH.a
                    [ HP.classes $ HH.ClassName <$> [ "navbar-item" ]
                    , HE.onClick \_ -> SetMenu false
                    , HP.href $ hashStr Index
                    ]
                    [ HH.text "Generate" ]
                , HH.a
                    [ HP.classes $ HH.ClassName <$> [ "navbar-item" ]
                    , HE.onClick \_ -> SetMenu false
                    , HP.href $ hashStr List
                    ]
                    [ HH.text "Gallery" ]
                ]
            , HH.div
                [ HP.classes $ HH.ClassName <$> [ "navbar-end" ] ]
                [ HH.div
                    [ HP.classes $ HH.ClassName <$> [ "navbar-item" ] ]
                    [ HH.div
                        [ HP.classes $ HH.ClassName <$> [ "buttons" ] ]
                        []
                    ]
                ]
            ]
        ]
    ]

handleAction :: forall o m. Action -> H.HalogenM State Action () o m Unit
handleAction = case _ of
  SetMenu flag -> do
    H.modify_ (_ { isMenuActive = flag })

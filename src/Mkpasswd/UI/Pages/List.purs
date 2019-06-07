module Mkpasswd.UI.Pages.List where

import Prelude
import Mkpasswd.Data.States       (FormData)
import Mkpasswd.Halogen.Util      (classes)
import Mkpasswd.UI.Components.HeaderNav as Nav
import Mkpasswd.UI.Element     as UI
import Mkpasswd.UI.Routing        (RouteHash(..), routeHref)
import Data.Array                 (mapWithIndex, snoc, null)
import Data.Maybe                 (Maybe(..))
import Effect.Aff                 (Aff)
import Halogen                 as H
import Halogen.HTML            as HH
import Halogen.HTML.Events     as HE
import Halogen.HTML.Properties as HP
import Web.HTML                as Web
import Web.HTML.Window         as Win

type ChildQuery = Nav.Query
type ChildSlot = Unit

type Input = Array FormData

data Message = DelPasswd Int

type State =
    { list  :: Array FormData
    , openMenuIndex :: Maybe Int
    }

data Query a
    = Delete Int a
    | ToggleMenu Int a
    | Receive Input a

ui :: H.Component HH.HTML Query Input Message Aff
ui =
  H.parentComponent
    { initialState
    , render
    , eval
    , receiver : HE.input Receive
    }
    where
          initialState :: Input -> State
          initialState = { list: _ , openMenuIndex: Nothing }

          render :: State -> H.ParentHTML Query ChildQuery ChildSlot Aff
          render state =
            HH.main_
              [ HH.slot unit Nav.component unit absurd
              , UI.container $
                ( if null state.list
                    then
                        [ HH.div
                            [ classes ["message"] ]
                            [ HH.div
                                [ classes ["message-body", "text-wrap"] ]
                                [ HH.text "There is no work" ]
                            ]
                        ]
                    else mapWithIndex (accountRow state.openMenuIndex) state.list
                )
                `snoc`
                HH.div
                    [ classes ["sticky-bottom", "p1", "is-pulled-right"] ]
                    [ HH.a
                        [ classes [ "button", "is-dark", "is-rounded" ]
                        , HP.href $ routeHref New
                        ]
                        [ HH.span
                            [ classes [ "icon" ] ]
                            [ HH.i
                                [ classes ["fas", "fa-plus"]
                                , HP.attr (HH.AttrName "aria-hidden") "true"
                                ] []
                            ]
                        ]
                    ]
              ]
              
          accountRow mi i fd =
            HH.div
                [ classes [ "card", "mb1" ] ]
                [ HH.header
                    [ classes ["card-header"] ]
                    [ HH.h2
                        [ classes ["card-header-title", "text-wrap"] ]
                        [ HH.text fd.account ]
                    , HH.div
                        [ classes
                            [ "card-header-icon"
                            , "dropdown"
                            , "is-right"
                            , if mi == Just i
                                then "is-active"
                                else ""
                            ]
                        ]
                        [ HH.a
                            [ classes ["dropdown-trigger"]
                            , HP.attr (HH.AttrName "aria-label") "more options"
                            , HE.onClick $ HE.input_ (ToggleMenu i)
                            ]
                            [ HH.span
                                [ classes ["icon"] ]
                                [ HH.i
                                    [ classes ["fas", "fa-ellipsis-v"]
                                    , HP.attr (HH.AttrName "aria-hidden") "true"
                                    ] []
                                ]
                            ]
                        , HH.div
                            [ classes ["dropdown-menu"] ]
                            [ HH.div
                                [ classes ["dropdown-content"] ]
                                [ HH.a
                                    [ classes ["dropdown-item"]
                                    , HP.href $ routeHref (Store i)
                                    , HE.onClick $ HE.input_ (ToggleMenu i)
                                    ]
                                    [ HH.span
                                        [ classes [ "icon" ] ]
                                        [ HH.i
                                            [ classes ["fas", "fa-pen-fancy"]
                                            , HP.attr (HH.AttrName "aria-hidden") "true"
                                            ] []
                                        ]
                                    , HH.text "edit"
                                    ]
                                , HH.a
                                    [ classes ["dropdown-item"]
                                    , HE.onClick (HE.input_ $ Delete i)
                                    ]
                                    [ HH.span
                                        [ classes [ "icon" ] ]
                                        [ HH.i
                                            [ classes ["fas", "fa-trash-alt"]
                                            , HP.attr (HH.AttrName "aria-hidden") "true"
                                            ] []
                                        ]
                                    , HH.text "remove"
                                    ]
                                ]
                            ]
                        ]
                    ]
                , HH.div
                    [ classes ["card-content"] ]
                    [ HH.div
                        [ classes ["message"] ]
                        [ HH.div
                            [ classes ["message-body", "text-wrap"] ]
                            [ HH.text fd.passwd ]
                        ]
                    , HH.div
                        [ classes ["text-wrap"] ]
                        [ HH.text fd.note ]
                    ]
                ]

          eval :: Query ~> H.ParentDSL State Query ChildQuery ChildSlot Message Aff
          eval (Delete i next) = do
             a <- H.liftEffect $ Web.window >>= Win.confirm "削除します。よろしいですか？" 
             when a $ H.raise $ DelPasswd i
             eval (ToggleMenu i next)
          eval (ToggleMenu i next) = do
             mi <- _.openMenuIndex <$> H.get
             H.modify_ (_ { openMenuIndex = if mi == Just i then Nothing else Just i})
             pure next
          eval (Receive s next) = do
             H.modify_ (_ { list = s })
             pure next

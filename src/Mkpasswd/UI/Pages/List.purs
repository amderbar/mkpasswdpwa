module Mkpasswd.UI.Pages.List where

import Prelude
import Mkpasswd.Data.States       (FormData)
import Mkpasswd.Halogen.Util      (classes)
import Mkpasswd.UI.Components.HeaderNav as Nav
import Mkpasswd.UI.Routing        (RouteHash(..), routeHref)
import Data.Array                 (mapWithIndex)
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
    }

data Query a
    = Delete Int a
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
          initialState = { list: _ }

          render :: State -> H.ParentHTML Query ChildQuery ChildSlot Aff
          render state =
            HH.main_
              [ HH.slot unit Nav.component unit absurd
              , HH.div
                    [ classes [ "flex-auto" , "flex", "flex-column" ] ]
                    [ HH.h1 [ classes [ "center" ] ] [ HH.text "List" ]
                    , HH.a
                        [ classes [ "mb1", "btn", "btn-primary", "self-center" ]
                        , HP.href $ routeHref New
                        ]
                        [ HH.text "new" ]
                    , HH.table
                        [ classes [ "col", "col-12", "border" ] ]
                        [ HH.thead_
                            [ HH.tr_
                                [ HH.td_ []
                                , HH.td_ [ HH.text "ID：" ]
                                , HH.td_ [ HH.text "パスワード：" ]
                                , HH.td_ [ HH.text "備考：" ]
                                ]
                            ]
                        , HH.tbody_ $ mapWithIndex accountRow state.list
                        ]
                    ]
              ]
          accountRow i fd =
                HH.tr_
                    [ HH.td
                        [ classes [ "border-top" ] ]
                        [ HH.a
                            [ classes [ "btn", "btn-small", "btn-primary" ]
                            , HP.href $ routeHref (Store i)
                            ]
                            [ HH.text "edit" ]
                        , HH.a
                            [ classes [ "ml1", "btn", "btn-small", "btn-outline" ]
                            , HE.onClick (HE.input_ $ Delete i)
                            ]
                            [ HH.text "del" ]
                        ]
                    , HH.td [ classes [ "border-top" ] ] [ HH.text fd.account ]
                    , HH.td [ classes [ "border-top" ] ] [ HH.text fd.passwd  ]
                    , HH.td [ classes [ "border-top" ] ] [ HH.text fd.note ]
                    ]

          eval :: Query ~> H.ParentDSL State Query ChildQuery ChildSlot Message Aff
          eval (Delete i next) = do
             a <- H.liftEffect $ Web.window >>= Win.confirm "削除します。よろしいですか？" 
             when a $ H.raise $ DelPasswd i
             pure next
          eval (Receive s next) = do
             H.modify_ (_ { list = s })
             pure next

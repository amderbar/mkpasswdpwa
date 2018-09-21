module Mkpasswd.UI.Pages.List where

import Prelude
import Mkpasswd.Data.States       (FormData)
import Mkpasswd.Halogen.Util      (classes)
import Mkpasswd.UI.Routing        (RouteHash(..), routeHref)
import Data.Array                 (mapWithIndex)
import Data.Maybe                 (Maybe(..))
import Effect.Aff                 (Aff)
import Halogen                 as H
import Halogen.HTML            as HH
import Halogen.HTML.Events     as HE
import Halogen.HTML.Properties as HP

type Input = Array FormData

type State =
    { list  :: Array FormData
    }

data Query a = Identity a

ui :: H.Component HH.HTML Query Input Void Aff
ui =
  H.component
    { initialState
    , render
    , eval
    , receiver : const Nothing
    }
    where
          initialState :: Input -> State
          initialState = { list: _ }

          render :: State -> H.ComponentHTML Query
          render state =
                HH.div
                    [ classes [ "flex-auto" , "flex", "flex-column" ] ]
                    [ HH.h1 [ classes [ "center" ] ] [ HH.text "List" ]
                    , HH.a
                        [ classes [ "mb1", "btn", "btn-primary", "self-center" ]
                        , HP.href $ routeHref New
                        ]
                        [ HH.text "new" ]
                    , HH.table
                        [ classes [ "self-center", "col", "col-11", "border" ] ]
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
          accountRow i fd =
                HH.tr_
                    [ HH.td
                        [ classes [ "border-top" ] ]
                        [ HH.a
                            [ classes [ "col", "col-12" ]
                            , HP.href $ routeHref (Store i)
                            ]
                            [ HH.text "edit" ]
                        ]
                    , HH.td [ classes [ "border-top" ] ] [ HH.text fd.account ]
                    , HH.td [ classes [ "border-top" ] ] [ HH.text fd.passwd  ]
                    , HH.td [ classes [ "border-top" ] ] [ HH.text fd.note ]
                    ]

          eval :: Query ~> H.ComponentDSL State Query Void Aff
          eval (Identity next) = pure next

module Mkpasswd.UI.Pages.List where

import Prelude
import Data.Array (mapWithIndex, null, catMaybes)
import Data.Maybe (Maybe(..))
import Data.Symbol (SProxy(..))
import Effect.Class (class MonadEffect)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Mkpasswd.Data.States (FormData)
import Mkpasswd.UI.Components.HeaderNav as Nav
import Mkpasswd.UI.Routing (RouteHash(..), routeHref)
import Web.HTML as Web
import Web.HTML.Window as Win

type Slot id = forall q. H.Slot q DeleteTargetIdx id

type Slots = ( headerNav :: Nav.Slot Unit )

_headerNav = SProxy :: SProxy "headerNav"

type Input = Array FormData

type DeleteTargetIdx = Int

type State =
  { list  :: Array FormData
  , openMenuIndex :: Maybe Int
  }

data Action
  = Delete Int
  | ToggleMenu Int
  | Receive Input

component :: forall q m. MonadEffect m => H.Component HH.HTML q Input DeleteTargetIdx m
component =
  H.mkComponent
  { initialState
  , render
  , eval: H.mkEval $ H.defaultEval
    { handleAction = handleAction
    , receive = Just <<< Receive
    }
  }

initialState :: Input -> State
initialState = { list: _ , openMenuIndex: Nothing }

render :: forall m. MonadEffect m => State -> H.ComponentHTML Action Slots m
render state =
  HH.main_
    [ HH.slot _headerNav unit Nav.component unit absurd
    , HH.section
      [ HP.classes $ HH.ClassName <$> [ "section" ] ]
      [ HH.div
        [ HP.classes $ HH.ClassName <$> [ "container" ] ] $
        join
          [ if null state.list
            then emptyListView
            else mapWithIndex (accountRow state.openMenuIndex) state.list
          , footerBtnArea
          ]
      ]
    ]
  
accountRow :: forall i. Maybe Int -> Int -> FormData -> HH.HTML i Action
accountRow mi i fd =
  HH.div
    [ HP.classes $ HH.ClassName <$> [ "card", "mb1" ] ]
    [ HH.header
      [ HP.classes $ HH.ClassName <$> ["card-header"] ]
      [ HH.h2
        [ HP.classes $ HH.ClassName <$> ["card-header-title", "text-wrap"] ]
        [ HH.text fd.account ]
      , cardMenu mi i
      ]
    , HH.div
      [ HP.classes $ HH.ClassName <$> ["card-content"] ]
      [ HH.div
        [ HP.classes $ HH.ClassName <$> ["message"] ]
        [ HH.div
          [ HP.classes $ HH.ClassName <$> ["message-body", "text-wrap"] ]
          [ HH.text fd.passwd ]
          ]
        , HH.div
          [ HP.classes $ HH.ClassName <$> ["text-wrap"] ]
          [ HH.text fd.note ]
      ]
    ]

cardMenu :: forall i. Maybe Int -> Int -> HH.HTML i Action
cardMenu mi i =
  HH.div
    [ HP.classes $ HH.ClassName <$>
      [ "card-header-icon"
      , "dropdown"
      , "is-right"
      , if mi == Just i then "is-active" else ""
      ]
    ]
    [ menuBtn i
    , HH.div
      [ HP.classes $ HH.ClassName <$> ["dropdown-menu"] ]
      [ HH.div
        [ HP.classes $ HH.ClassName <$> ["dropdown-content"] ]
        [ dropdownItem (Just $ Store i) (ToggleMenu i) "fa-pen-fancy" "edit"
        , dropdownItem Nothing (Delete i) "fa-trash-alt" "remove"
        ]
      ]
    ]

menuBtn :: forall i. Int -> HH.HTML i Action
menuBtn i =
  HH.a
    [ HP.classes $ HH.ClassName <$> ["dropdown-trigger"]
    , HP.attr (HH.AttrName "aria-label") "more options"
    , HE.onClick $ \_ -> Just (ToggleMenu i)
    ]
    [ HH.span
      [ HP.classes $ HH.ClassName <$> ["icon"] ]
      [ HH.i
        [ HP.classes $ HH.ClassName <$> ["fas", "fa-ellipsis-v"]
        , HP.attr (HH.AttrName "aria-hidden") "true"
        ] []
      ]
    ]

dropdownItem :: forall i. Maybe RouteHash -> Action -> String -> String -> HH.HTML i Action
dropdownItem mHref action icon label =
  HH.a
    ( catMaybes
      [ Just $ HP.classes $ HH.ClassName <$> ["dropdown-item"]
      , (HP.href <<< routeHref) <$> mHref
      , Just $ HE.onClick  \_ -> Just action
      ]
    )
    [ HH.span
      [ HP.classes $ HH.ClassName <$> [ "icon" ] ]
      [ HH.i
        [ HP.classes $ HH.ClassName <$> ["fas", icon]
        , HP.attr (HH.AttrName "aria-hidden") "true"
        ] []
      ]
    , HH.text label
    ]

emptyListView :: forall i p. Array (HH.HTML i p)
emptyListView =
  [ HH.div
    [ HP.classes $ HH.ClassName <$> ["message"] ]
    [ HH.div
      [ HP.classes $ HH.ClassName <$> ["message-body", "text-wrap"] ]
      [ HH.text "There is no work" ]
    ]
  ]

footerBtnArea :: forall i p. Array (HH.HTML i p)
footerBtnArea =
  [ HH.div
    [ HP.classes $ HH.ClassName <$> ["sticky-bottom", "p1", "is-pulled-right"] ]
    [ HH.a
      [ HP.classes $ HH.ClassName <$> [ "button", "is-dark", "is-rounded" ]
      , HP.href $ routeHref New
      ]
      [ HH.span
        [ HP.classes $ HH.ClassName <$> [ "icon" ] ]
        [ HH.i
          [ HP.classes $ HH.ClassName <$> ["fas", "fa-plus"]
          , HP.attr (HH.AttrName "aria-hidden") "true"
          ] []
        ]
      ]
    ]
  ]

handleAction :: forall m. MonadEffect m => Action -> H.HalogenM State Action Slots DeleteTargetIdx m Unit
handleAction = case _ of
  Delete i -> do
    a <- H.liftEffect $ Web.window >>= Win.confirm "削除します。よろしいですか？" 
    when a $ H.raise i
    handleAction (ToggleMenu i)

  ToggleMenu i -> do
    mi <- _.openMenuIndex <$> H.get
    H.modify_ (_ { openMenuIndex = if mi == Just i then Nothing else Just i})

  Receive s -> do
    H.modify_ (_ { list = s })

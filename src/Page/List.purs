module Page.List where

import Prelude
import Data.Array (mapWithIndex, null, catMaybes)
import Data.Foldable (for_)
import Data.Maybe (Maybe(..))
import Type.Proxy (Proxy(..))
import Effect.Class (class MonadEffect)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Data.Csv (encodeCsv, formDataToRows)
import Data.States (FormData)
import Component.HeaderNav as Nav
import Effect.Download (csvObjectUrl)
import Effect.Routing (RouteHash, hashStr)
import Web.File.Url as Url
import Web.HTML as Web
import Web.HTML.Window as Win

type Slot id = forall q. H.Slot q DeleteTargetIdx id

type Input = Array FormData

type DeleteTargetIdx = Int

type CsvExport = { url :: String, filename :: String }

type State =
  { list :: Array FormData
  , openMenuIndex :: Maybe Int
  , csvExport :: Maybe CsvExport
  }

data Action
  = Delete Int
  | ToggleMenu Int
  | Receive Input
  | GenerateCsv
  | CloseCsvExport

component :: forall q m. MonadEffect m => H.Component q Input DeleteTargetIdx m
component =
  H.mkComponent
    { initialState
    , render
    , eval:
        H.mkEval
          $ H.defaultEval
              { handleAction = handleAction
              , receive = Just <<< Receive
              , finalize = Just CloseCsvExport
              }
    }

initialState :: Input -> State
initialState = { list: _, openMenuIndex: Nothing, csvExport: Nothing }

type ChildSlots = (headerNav :: Nav.Slot Unit)

_headerNav = Proxy :: Proxy "headerNav"

render :: forall m. MonadEffect m => State -> H.ComponentHTML Action ChildSlots m
render state =
  HH.main_
    [ HH.slot _headerNav unit Nav.component unit absurd
    , HH.section
        [ HP.classes $ HH.ClassName <$> [ "section" ] ]
        [ HH.div
            [ HP.classes $ HH.ClassName <$> [ "container" ] ]
            $ join
                [ [ deprecationBanner ]
                , if null state.list then
                    emptyListView
                  else
                    mapWithIndex (accountRow state.openMenuIndex) state.list
                , footerBtnArea state.list
                ]
        ]
    , csvExportModal state.csvExport
    ]

deprecationBanner :: forall i p. HH.HTML i p
deprecationBanner =
  HH.div
    [ HP.classes $ HH.ClassName <$> [ "notification", "is-warning", "is-light" ] ]
    [ HH.p
        [ HP.classes $ HH.ClassName <$> [ "mb0" ] ]
        [ HH.text "この機能は将来廃止予定です。保存済みデータはCSV出力で保存し、別ツールへの移行をご検討ください。" ]
    ]

accountRow :: forall i. Maybe Int -> Int -> FormData -> HH.HTML i Action
accountRow mi i fd =
  HH.div
    [ HP.classes $ HH.ClassName <$> [ "card", "mb1" ] ]
    [ HH.header
        [ HP.classes $ HH.ClassName <$> [ "card-header" ] ]
        [ HH.h2
            [ HP.classes $ HH.ClassName <$> [ "card-header-title", "text-wrap" ] ]
            [ HH.text fd.account ]
        , cardMenu mi i
        ]
    , HH.div
        [ HP.classes $ HH.ClassName <$> [ "card-content" ] ]
        [ HH.div
            [ HP.classes $ HH.ClassName <$> [ "message" ] ]
            [ HH.div
                [ HP.classes $ HH.ClassName <$> [ "message-body", "text-wrap" ] ]
                [ HH.text fd.passwd ]
            ]
        , HH.div
            [ HP.classes $ HH.ClassName <$> [ "text-wrap" ] ]
            [ HH.text fd.note ]
        ]
    ]

cardMenu :: forall i. Maybe Int -> Int -> HH.HTML i Action
cardMenu mi i =
  HH.div
    [ HP.classes $ HH.ClassName
        <$>
          [ "card-header-icon"
          , "dropdown"
          , "is-right"
          , if mi == Just i then "is-active" else ""
          ]
    ]
    [ menuBtn i
    , HH.div
        [ HP.classes $ HH.ClassName <$> [ "dropdown-menu" ] ]
        [ HH.div
            [ HP.classes $ HH.ClassName <$> [ "dropdown-content" ] ]
            [ dropdownItem Nothing (ToggleMenu i) "fa-pen-fancy" "edit"
            , dropdownItem Nothing (Delete i) "fa-trash-alt" "remove"
            ]
        ]
    ]

menuBtn :: forall i. Int -> HH.HTML i Action
menuBtn i =
  HH.a
    [ HP.classes $ HH.ClassName <$> [ "dropdown-trigger" ]
    , HP.attr (HH.AttrName "aria-label") "more options"
    , HE.onClick $ \_ -> ToggleMenu i
    ]
    [ HH.span
        [ HP.classes $ HH.ClassName <$> [ "icon" ] ]
        [ HH.i
            [ HP.classes $ HH.ClassName <$> [ "fas", "fa-ellipsis-v" ]
            , HP.attr (HH.AttrName "aria-hidden") "true"
            ]
            []
        ]
    ]

dropdownItem :: forall i. Maybe RouteHash -> Action -> String -> String -> HH.HTML i Action
dropdownItem mHref action icon label =
  HH.a
    ( catMaybes
        [ Just $ HP.classes $ HH.ClassName <$> [ "dropdown-item" ]
        , (HP.href <<< hashStr) <$> mHref
        , Just $ HE.onClick \_ -> action
        ]
    )
    [ HH.span
        [ HP.classes $ HH.ClassName <$> [ "icon" ] ]
        [ HH.i
            [ HP.classes $ HH.ClassName <$> [ "fas", icon ]
            , HP.attr (HH.AttrName "aria-hidden") "true"
            ]
            []
        ]
    , HH.text label
    ]

emptyListView :: forall i p. Array (HH.HTML i p)
emptyListView =
  [ HH.div
      [ HP.classes $ HH.ClassName <$> [ "message" ] ]
      [ HH.div
          [ HP.classes $ HH.ClassName <$> [ "message-body", "text-wrap" ] ]
          [ HH.text "There is no work" ]
      ]
  ]

footerBtnArea :: forall i. Array FormData -> Array (HH.HTML i Action)
footerBtnArea list =
  [ HH.div
      [ HP.classes $ HH.ClassName <$> [ "sticky-bottom", "p1", "is-pulled-right" ] ]
      [ HH.button
          [ HP.classes $ HH.ClassName <$> [ "button", "is-rounded", "mr1" ]
          , HP.disabled (null list)
          , HE.onClick \_ -> GenerateCsv
          ]
          [ HH.text "CSVを生成" ]
      , HH.a
          [ HP.classes $ HH.ClassName <$> [ "button", "is-dark", "is-rounded" ]
          , HP.attr (HH.AttrName "aria-disabled") "true"
          , HP.attr (HH.AttrName "title") "新規登録は廃止予定です。既存データはCSV出力でお手元に保存できます。"
          ]
          [ HH.span
              [ HP.classes $ HH.ClassName <$> [ "icon" ] ]
              [ HH.i
                  [ HP.classes $ HH.ClassName <$> [ "fas", "fa-plus" ]
                  , HP.attr (HH.AttrName "aria-hidden") "true"
                  ]
                  []
              ]
          ]
      ]
  ]

csvExportModal :: forall i. Maybe CsvExport -> HH.HTML i Action
csvExportModal = case _ of
  Nothing -> HH.text ""
  Just { url, filename } ->
    HH.div
      [ HP.classes $ HH.ClassName <$> [ "modal", "is-active" ] ]
      [ HH.div [ HP.classes $ HH.ClassName <$> [ "modal-background" ] ] []
      , HH.div
          [ HP.classes $ HH.ClassName <$> [ "modal-content" ] ]
          [ HH.div
              [ HP.classes $ HH.ClassName <$> [ "box" ] ]
              [ HH.p_ [ HH.text "CSVファイルの準備ができました。" ]
              , HH.p_ [ HH.text "パスワードを平文で含むファイルです。取り扱いにご注意ください。" ]
              , HH.div
                  [ HP.classes $ HH.ClassName <$> [ "buttons" ] ]
                  [ HH.a
                      [ HP.classes $ HH.ClassName <$> [ "button", "is-success" ]
                      , HP.href url
                      , HP.attr (HH.AttrName "download") filename
                      ]
                      [ HH.text "ダウンロード" ]
                  , HH.button
                      [ HP.classes $ HH.ClassName <$> [ "button" ]
                      , HE.onClick \_ -> CloseCsvExport
                      ]
                      [ HH.text "close" ]
                  ]
              ]
          ]
      ]

handleAction :: forall m. MonadEffect m => Action -> H.HalogenM State Action ChildSlots DeleteTargetIdx m Unit
handleAction = case _ of
  Delete i -> do
    a <- H.liftEffect $ Web.window >>= Win.confirm "削除します。よろしいですか？"
    when a $ H.raise i
    handleAction (ToggleMenu i)
  ToggleMenu i -> do
    mi <- _.openMenuIndex <$> H.get
    H.modify_ (_ { openMenuIndex = if mi == Just i then Nothing else Just i })
  Receive s -> do
    H.modify_ (_ { list = s })
  GenerateCsv -> do
    list <- _.list <$> H.get
    url <- H.liftEffect $ csvObjectUrl $ encodeCsv $ formDataToRows list
    H.modify_ (_ { csvExport = Just { url, filename: "mkpasswdpwa.csv" } })
  CloseCsvExport -> do
    mExport <- _.csvExport <$> H.get
    for_ mExport \{ url } -> H.liftEffect $ Url.revokeObjectURL url
    H.modify_ (_ { csvExport = Nothing })

module Mkpasswd.UI.Pages.Store where

import Prelude
import Mkpasswd.Data.States       (FormData, initialForm)
import Mkpasswd.Data.Validation   (ErrorCode(..), chk, maxRule, requiredRule)
import Mkpasswd.Halogen.Util      (classes)
import Mkpasswd.UI.Components.HeaderNav as Nav
import Mkpasswd.UI.Element     as UI
import Mkpasswd.UI.Routing        (RouteHash(..), routeHref)
import Data.Const                 (Const)
import Data.Generic.Rep           (class Generic)
import Data.Generic.Rep.Show      (genericShow)
import Data.Maybe                 (Maybe(..), fromMaybe, isJust)
import Data.Newtype               (class Newtype)
import Data.String                (length)
import Effect.Aff                 (Aff)
import Formless                as F
import Halogen                 as H
import Halogen.Component.ChildPath as HC
import Halogen.Data.Prism         (type (<\/>), type (\/))
import Halogen.HTML            as HH
import Halogen.HTML.Events     as HE
import Halogen.HTML.Properties as HP
import Routing.Hash               (setHash)

type FormlessQuery = F.Query' Form Aff

type ChildQuery = Nav.Query <\/> FormlessQuery <\/> Const Void
type ChildSlot = Unit \/ Unit \/ Void

cpNav :: HC.ChildPath Nav.Query ChildQuery Unit ChildSlot
cpNav = HC.cp1

cpFrm :: HC.ChildPath FormlessQuery ChildQuery Unit ChildSlot
cpFrm = HC.cp2

type Input = Maybe FormData

data Message = SavePasswd FormData

type State = FormData

data FeildType
    = AccountInput
    | Passwdinput
    | NoteTextarea

derive instance genericFeildType :: Generic FeildType _
instance showFeildType :: Show FeildType where
    show = genericShow

data Query a
    = Formless (F.Message' Form) a

ui :: H.Component HH.HTML Query Input Message Aff
ui =
  H.parentComponent
    { initialState
    , render
    , eval
    , receiver : const Nothing
    }
    where
          initialState :: Input -> State
          initialState inp = fromMaybe initialForm inp

          render :: State -> H.ParentHTML Query ChildQuery ChildSlot Aff
          render state =
            HH.main_
              [ HH.slot' cpNav unit Nav.component unit absurd
              , HH.slot' cpFrm unit F.component
                { initialInputs: F.wrapInputFields state, validators, render: renderFormless }
                (HE.input Formless)
              ]

          eval :: Query ~> H.ParentDSL State Query ChildQuery ChildSlot Message Aff
          eval (Formless m a) = case m of
             F.Submitted formOutput -> a <$ do
                let form = F.unwrapOutputFields formOutput
                H.liftEffect $ setHash $ routeHref List
                H.raise $ SavePasswd form
             _ -> pure a


-- Formless

newtype Form r f = Form (r
  ( account :: f (Array ErrorCode) String String
  , passwd  :: f (Array ErrorCode) String String
  , note    :: f (Array ErrorCode) String String
  ))
derive instance newtypeForm :: Newtype (Form r f) _

proxy :: F.SProxies Form
proxy = F.mkSProxies (F.FormProxy :: F.FormProxy Form)

validators :: Form Record (F.Validation Form Aff)
validators = Form
  { account: isNonEmpty >>> isUnder100
  , passwd : isNonEmpty >>> isUnder100
  , note   : isUnder1000
  }
  where
    isNonEmpty  = F.hoistFnE_ (chk ValueMissing requiredRule)
    isUnder100  = F.hoistFnE_ (\v -> chk OutOfRange (maxRule 100) (length v) *> pure v)
    isUnder1000 = F.hoistFnE_ (\v -> chk OutOfRange (maxRule 1000) (length v) *> pure v)

renderFormless :: forall m. F.State Form m -> F.HTML' Form m
renderFormless fstate =
  UI.container
    [ HH.div
        [ classes [ "field" ] ]
        [ HH.label
            [ HP.for $ show AccountInput
            , classes [ "label" ]
            ]
            [ HH.text "Title" ]
        , HH.div
            [ classes ["control"] ]
            [ HH.input
                [ HP.type_ HP.InputText
                , HP.id_ $ show AccountInput
                , classes
                    [ "input"
                    , if (isJust $ F.getError proxy.account fstate.form)
                        then "is-danger"
                        else ""
                    ]
                , HP.value $ F.getInput proxy.account fstate.form
                , HE.onValueInput $ HE.input $ F.setValidate proxy.account
                ]
            ]
        , HH.ul
            [ classes ["help", "is-danger"] ]
            $ (\msg -> HH.li_ [ HH.text (errorMsg msg) ])
            <$> (fromMaybe [] $ F.getError proxy.account fstate.form)
        ]
    , HH.div
        [ classes [ "field" ] ]
        [ HH.label
            [ HP.for $ show Passwdinput
            , classes [ "label" ]
            ]
            [ HH.text "The Work" ]
        , HH.div
            [ classes ["control"] ]
            [ HH.input
                [ HP.type_ HP.InputText
                , HP.id_ $ show Passwdinput
                , classes
                    [ "input"
                    , if (isJust $ F.getError proxy.passwd fstate.form)
                        then "is-danger"
                        else ""
                    ]
                , HP.value $ F.getInput proxy.passwd fstate.form
                , HE.onValueInput $ HE.input $ F.setValidate proxy.passwd
                ]
            ]
        , HH.ul
            [ classes ["help", "is-danger"] ]
            $ (\msg -> HH.li_ [ HH.text (errorMsg msg) ])
            <$> (fromMaybe [] $ F.getError proxy.passwd fstate.form)
        ]
    , HH.div
        [ classes [ "field" ] ]
        [ HH.label
            [ HP.for $ show NoteTextarea
            , classes [ "label" ]
            ]
            [ HH.text "Description" ]
        , HH.div
            [ classes ["control"] ]
            [ HH.textarea
                [ HP.id_ $ show NoteTextarea
                , classes
                    [ "textarea"
                    , if (isJust $ F.getError proxy.note fstate.form)
                        then "is-danger"
                        else ""
                    ]
                , HP.value $ F.getInput proxy.note fstate.form
                , HE.onValueInput $ HE.input $ F.setValidate proxy.note
                ]
            ]
        , HH.ul
            [ classes ["help", "is-danger"] ]
            $ (\msg -> HH.li_ [ HH.text (errorMsg msg) ])
            <$> (fromMaybe [] $ F.getError proxy.note fstate.form)
        ]
    , HH.div
        [ classes [ "field", "is-grouped" ] ]
        [ HH.span
            [ classes [ "control" ] ]
            [ HH.button
                [ classes [ "button", "is-dark" ]
                , HE.onClick $ HE.input_ F.Submit
                ]
                [ HH.text "Save" ]
            ]
        , HH.span
            [ classes [ "control" ] ]
            [ HH.a
                [ classes [ "button" ]
                , HP.href $ routeHref List
                ]
                [ HH.text "Cancel" ]
            ]
        ]
    ]
  where
    errorMsg OutOfRange   = "長過ぎます"
    errorMsg ValueMissing = "入力してください"
    errorMsg _            = "なんかエラーになったんでリロードしてください"

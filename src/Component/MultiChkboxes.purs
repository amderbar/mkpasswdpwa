module Mkpasswd.Component.MultiChkboxes where

import Prelude
import Data.Array ((:), mapWithIndex, modifyAt, catMaybes)
import Data.Array.NonEmpty (NonEmptyArray, fromArray)
import Data.Char.Gen.Symbols (symbols)
import Data.Const (Const)
import Data.Either (note)
import Data.Generic.Rep (class Generic)
import Data.Show.Generic (genericShow)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Newtype (class Newtype)
import Data.String.CodeUnits (singleton)
import Data.Switch (Switch, toSwitch)
import Data.Switch as Switch
import Effect.Aff.Class (class MonadAff)
import Formless as F
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Type.Proxy (Proxy(..))
import Web.Event.Event (Event)

type Slot
  = F.Slot' Form AllowedSymbols

type State
  = ( allChk :: Boolean
    , isOpen :: Boolean
    , disabled :: Boolean
    )

type AllowedSymbols
  = { allowedSymbols :: NonEmptyArray Char }

newtype Form (r :: Row Type -> Type) f
  = Form
  ( r
      ( allowedSymbols :: f (Array ErrorCode) (Array (Switch Char)) (NonEmptyArray Char) )
  )

_allowedSymbols = Proxy :: Proxy "allowedSymbols"

derive instance newtypeForm :: Newtype (Form r f) _

data ErrorCode
  = EmptyCharSet

derive instance genericErrorReason :: Generic ErrorCode _

instance showErrorReason :: Show ErrorCode where
  show = genericShow

data Action
  = SetDisabled Boolean
  | ToggleOpen
  | ToggleAllChk
  | ToggleCharSwtich Int

type FormAction
  = F.Action Form Action

component :: forall m. MonadAff m => F.Component Form (Const Void) () Boolean AllowedSymbols m
component =
  F.component formInput
    $ F.defaultSpec
        { render = render
        , handleAction = handleAction
        , handleEvent = F.raiseResult
        , receive = Just <<< SetDisabled
        }

formInput :: forall m. MonadAff m => Boolean -> F.Input Form State m
formInput disabled =
  { allChk: true
  , isOpen: false
  , disabled: disabled
  , initialInputs:
      Just $ F.wrapInputFields
        $ { allowedSymbols: toSwitch true <$> symbols }
  , validators:
      Form
        { allowedSymbols: transformAllowedSymbols }
  }
  where
  transformAllowedSymbols =
    F.hoistFnE_ \arr ->
      let
        seed = (fromArray <<< catMaybes) $ Switch.toMaybe <$> arr
      in
        note [ EmptyCharSet ] seed

handleAction :: forall m. MonadAff m => Action -> F.HalogenM Form State Action () AllowedSymbols m Unit
handleAction = case _ of
  SetDisabled disabled -> do
    H.modify_ _ { disabled = disabled }
  ToggleOpen -> do
    isOpen <- H.gets _.isOpen
    H.modify_ _ { isOpen = not isOpen }
  ToggleAllChk -> do
    allChk <- not <$> H.gets _.allChk
    H.modify_ _ { allChk = allChk }
    form <- H.gets _.form
    let
      allowedSymbols = (if allChk then Switch.on else Switch.off) <$> F.getInput _allowedSymbols form
    F.handleAction handleAction F.raiseResult $ F.setValidate _allowedSymbols allowedSymbols
  ToggleCharSwtich idx -> do
    form <- H.gets _.form
    let
      allowedSymbols = (\i f a -> fromMaybe a $ modifyAt i f a) idx Switch.toggle $ F.getInput _allowedSymbols form
    F.handleAction handleAction F.raiseResult $ F.setValidate _allowedSymbols allowedSymbols

render :: forall m. MonadAff m => F.PublicState Form State -> F.ComponentHTML Form Action () m
render fstate =
  HH.div
    [ HP.classes $ HH.ClassName <$> [ "card" ] ]
    [ HH.header
        [ HP.classes $ HH.ClassName <$> [ "card-header" ]
        , HE.onClick \_ -> F.injAction ToggleOpen
        ]
        [ HH.p
            [ HP.classes $ HH.ClassName <$> [ "card-header-title" ] ]
            [ HH.text "Individual symbol" ]
        , HH.a
            [ HP.classes $ HH.ClassName <$> [ "card-header-icon" ]
            , HP.attr (HH.AttrName "aria-label") "more options"
            ]
            [ HH.span
                [ HP.classes $ HH.ClassName <$> [ "icon" ] ]
                [ HH.i
                    [ HP.classes $ HH.ClassName <$> [ "fas", "fa-angle-down" ]
                    , HP.attr (HH.AttrName "aria-hidden") "true"
                    ]
                    []
                ]
            ]
        ]
    , case (F.getError _allowedSymbols fstate.form) of
        Just err ->
          HH.div
            [ HP.classes $ HH.ClassName <$> [ "card-content" ] ]
            [ errorDisplay (Just err) ]
        Nothing ->
          HH.text ""
    , if fstate.isOpen then
        HH.fieldset
          [ HP.classes $ HH.ClassName <$> [ "card-content" ]
          , HP.disabled fstate.disabled
          ]
          $ checkboxRow fstate.disabled fstate.allChk (\_ -> F.injAction ToggleAllChk) (HH.text "use all symbol")
          : HH.hr_
          : flip mapWithIndex (F.getInput _allowedSymbols fstate.form)
              ( \i c ->
                  checkboxRow fstate.disabled (Switch.isOn c) (\_ -> F.injAction $ ToggleCharSwtich i)
                    $ HH.strong_ [ HH.text $ singleton (Switch.label c) ]
              )
      else
        HH.text ""
    ]
  where
  checkboxRow :: forall slot. Boolean -> Boolean -> (Event -> FormAction) -> HH.HTML slot FormAction -> HH.HTML slot FormAction
  checkboxRow isDisabled isChked onChange textElem =
    HH.div
      [ HP.classes $ HH.ClassName <$> [ "field" ] ]
      [ HH.span
          [ HP.classes $ HH.ClassName <$> [ "controll" ] ]
          [ HH.label
              [ HP.classes $ HH.ClassName <$> [ "checkbox" ] ]
              [ HH.input
                  [ HP.type_ HP.InputCheckbox
                  , HP.checked isChked
                  , HP.classes $ HH.ClassName <$> [ "mr1" ]
                  , HP.disabled isDisabled
                  , HE.onChange onChange
                  ]
              , textElem
              ]
          ]
      ]

  errorMsg :: ErrorCode -> String
  errorMsg = case _ of
    EmptyCharSet -> "使用する文字を指定してください"

  errorDisplay :: forall slot. Maybe (Array ErrorCode) -> HH.HTML slot FormAction
  errorDisplay err =
    HH.ul
      [ HP.classes $ HH.ClassName <$> [ "help", "is-danger" ] ]
      $ (\msg -> HH.li_ [ HH.text (errorMsg msg) ])
      <$> (fromMaybe [] $ err)

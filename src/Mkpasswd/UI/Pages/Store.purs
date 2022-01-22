module Mkpasswd.UI.Pages.Store where

import Prelude
import Data.Either (Either(..))
import Data.Generic.Rep (class Generic)
import Data.Show.Generic (genericShow)
import Data.Maybe (Maybe, fromMaybe, isJust)
import Data.Newtype (class Newtype)
import Data.String as Str
import Type.Proxy (Proxy(..))
import Effect.Aff.Class (class MonadAff)
import Formless as F
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Mkpasswd.Data.States (FormData)
import Mkpasswd.UI.Components.HeaderNav as Nav
import Mkpasswd.Data.Routing (RouteHash(..), hashStr)
import Routing.Hash (setHash)

type Slot id
  = forall q. H.Slot q NewFormData id

type Slots
  = ( headerNav :: Nav.Slot Unit
    , formless :: F.Slot' Form FormData Unit
    )

_headerNav = Proxy :: Proxy "headerNav"

type NewFormData
  = FormData

data Action
  = HandleFormless FormData

component :: forall q m. MonadAff m => H.Component q (Maybe FormData) NewFormData m
component =
  H.mkComponent
    { initialState
    , render
    , eval: H.mkEval $ H.defaultEval { handleAction = handleAction }
    }
  where
  initialState :: Maybe FormData -> Maybe FormData
  initialState = identity

  render :: Maybe FormData -> H.ComponentHTML Action Slots m
  render state =
    HH.main_
      [ HH.slot _headerNav unit Nav.component unit absurd
      , HH.section
          [ HP.classes $ HH.ClassName <$> [ "section" ] ]
          [ HH.slot F._formless unit (F.component formInput spec) state HandleFormless ]
      ]

  handleAction :: Action -> H.HalogenM (Maybe FormData) Action Slots NewFormData m Unit
  handleAction = case _ of
    HandleFormless fd -> do
      H.liftEffect $ setHash $ hashStr List
      H.raise fd

-- Formless
data ErrorCode
  = OutOfRange
  | ValueMissing

derive instance genericErrorReason :: Generic ErrorCode _

instance showErrorReason :: Show ErrorCode where
  show = genericShow

newtype Form (r :: Row Type -> Type) f
  = Form
  ( r
      ( account :: f (Array ErrorCode) String String
      , passwd :: f (Array ErrorCode) String String
      , note :: f (Array ErrorCode) String String
      )
  )

derive instance newtypeForm :: Newtype (Form r f) _

formInput :: forall m. Monad m => Maybe FormData -> F.Input' Form m
formInput fd =
  { initialInputs: F.wrapInputFields <$> fd
  , validators:
      Form
        { account: isNonEmpty >>> isUnder100
        , passwd: isNonEmpty >>> isUnder100
        , note: isUnder1000
        }
  }
  where
  chk :: forall e v. e -> (v -> Boolean) -> v -> Either (Array e) v
  chk e r v = if r v then pure v else Left [ e ]

  isNonEmpty = F.hoistFnE_ (chk ValueMissing $ not Str.null)

  isUnder100 = F.hoistFnE_ (\v -> chk OutOfRange (_ <= 100) (Str.length v) *> pure v)

  isUnder1000 = F.hoistFnE_ (\v -> chk OutOfRange (_ <= 1000) (Str.length v) *> pure v)

data FieldType
  = AccountInput
  | Passwdinput
  | NoteTextarea

derive instance genericFieldType :: Generic FieldType _

instance showFieldType :: Show FieldType where
  show = genericShow

spec :: forall i m. Monad m => MonadAff m => F.Spec' Form FormData i m
spec =
  F.defaultSpec
    { render = render
    , handleEvent = F.raiseResult
    }
  where
  _account = Proxy :: Proxy "account"

  _passwd = Proxy :: Proxy "passwd"

  _note = Proxy :: Proxy "note"

  errorMsg :: ErrorCode -> String
  errorMsg = case _ of
    OutOfRange -> "長過ぎます"
    ValueMissing -> "入力してください"

  render :: forall st. F.PublicState Form st -> F.ComponentHTML' Form m
  render fstate =
    HH.div
      [ HP.classes $ HH.ClassName <$> [ "container" ] ]
      [ HH.div
          [ HP.classes $ HH.ClassName <$> [ "field" ] ]
          [ labelBlock AccountInput "Title"
          , inputTextForm AccountInput (F.getError _account fstate.form) (F.getInput _account fstate.form) (F.setValidate _account)
          , errorDisplay (F.getError _account fstate.form)
          ]
      , HH.div
          [ HP.classes $ HH.ClassName <$> [ "field" ] ]
          [ labelBlock Passwdinput "The Work"
          , inputTextForm Passwdinput (F.getError _passwd fstate.form) (F.getInput _passwd fstate.form) (F.setValidate _passwd)
          , errorDisplay (F.getError _passwd fstate.form)
          ]
      , HH.div
          [ HP.classes $ HH.ClassName <$> [ "field" ] ]
          [ labelBlock NoteTextarea "Description"
          , textAreaForm NoteTextarea (F.getError _note fstate.form) (F.getInput _note fstate.form) (F.setValidate _note)
          , errorDisplay (F.getError _note fstate.form)
          ]
      , HH.div
          [ HP.classes $ HH.ClassName <$> [ "field", "is-grouped" ] ]
          [ HH.span
              [ HP.classes $ HH.ClassName <$> [ "control" ] ]
              [ HH.button
                  [ HP.classes $ HH.ClassName <$> [ "button", "is-dark" ]
                  , HE.onClick \_ -> F.submit
                  ]
                  [ HH.text "Save" ]
              ]
          , HH.span
              [ HP.classes $ HH.ClassName <$> [ "control" ] ]
              [ HH.a
                  [ HP.classes $ HH.ClassName <$> [ "button" ]
                  , HP.href $ hashStr List
                  ]
                  [ HH.text "Cancel" ]
              ]
          ]
      ]

  labelBlock :: forall slot. FieldType -> String -> HH.HTML slot (F.Action' Form)
  labelBlock fieldType labelTxt =
    HH.label
      [ HP.for $ show fieldType
      , HP.classes $ HH.ClassName <$> [ "label" ]
      ]
      [ HH.text labelTxt ]

  inputTextForm ::
    FieldType ->
    Maybe (Array ErrorCode) ->
    String ->
    (String -> F.Action' Form) ->
    F.ComponentHTML' Form m
  inputTextForm fieldType err inp onInput =
    HH.div
      [ HP.classes $ HH.ClassName <$> [ "control" ] ]
      [ HH.input
          [ HP.type_ HP.InputText
          , HP.id $ show fieldType
          , HP.classes $ HH.ClassName
              <$> [ "input"
                , if (isJust err) then
                    "is-danger"
                  else
                    ""
                ]
          , HP.value inp
          , HE.onValueInput onInput
          ]
      ]

  textAreaForm ::
    FieldType ->
    Maybe (Array ErrorCode) ->
    String ->
    (String -> F.Action' Form) ->
    F.ComponentHTML' Form m
  textAreaForm fieldType err inp onInput =
    HH.div
      [ HP.classes $ HH.ClassName <$> [ "control" ] ]
      [ HH.textarea
          [ HP.id $ show fieldType
          , HP.classes $ HH.ClassName
              <$> [ "textarea"
                , if (isJust err) then
                    "is-danger"
                  else
                    ""
                ]
          , HP.value inp
          , HE.onValueInput onInput
          ]
      ]

  errorDisplay :: forall slot. Maybe (Array ErrorCode) -> HH.HTML slot (F.Action' Form)
  errorDisplay err =
    HH.ul
      [ HP.classes $ HH.ClassName <$> [ "help", "is-danger" ] ]
      $ (\msg -> HH.li_ [ HH.text (errorMsg msg) ])
      <$> (fromMaybe [] $ err)

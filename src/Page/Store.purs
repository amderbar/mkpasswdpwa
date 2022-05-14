module Page.Store where

import Prelude
import Component.HeaderNav as Nav
import Component.RenderUtil (classes, inputFormContainer, inputTextForm, textAreaForm)
import Data.Array (null)
import Data.Either (Either(..))
import Data.Generic.Rep (class Generic)
import Data.Maybe (Maybe(..))
import Data.Routing (RouteHash(..), hashStr)
import Data.Show.Generic (genericShow)
import Data.States (FormData)
import Data.String as Str
import Effect.Aff.Class (class MonadAff)
import Formless as F
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Routing.Hash (setHash)
import Type.Proxy (Proxy(..))

type Slot id
  = forall q. H.Slot q Output id

type Slots
  = ( headerNav :: Nav.Slot Unit )

_headerNav = Proxy :: Proxy "headerNav"

type Form (f :: Type -> Type -> Type -> Type)
  = ( account :: f String ErrorCode String
    , passwd :: f String ErrorCode String
    , note :: f String ErrorCode String
    )

type FieldState
  = Form F.FieldState

type FieldAction
  = Form (F.FieldAction Action)

type FieldInput
  = Form F.FieldInput

type FieldResult
  = Form F.FieldResult

type FieldOutput
  = Form F.FieldOutput

type FormQuery query
  = F.FormQuery query FieldInput FieldResult FieldOutput

type FormOutput
  = F.FormOutput FieldState Output

type FormContext
  = F.FormContext FieldState FieldAction Input Action

type State
  = FormContext

type Input
  = Maybe FormData

type Output
  = FormData

data Action
  = Receive FormContext
  | Eval (F.FormlessAction FieldState)

component :: forall q m. MonadAff m => H.Component q Input Output m
component =
  (\c -> F.formless c initialValues innerComponent)
    { liftAction: Eval
    , validateOnChange: true
    }
  where
  initialValues :: { | Form F.FieldInput }
  initialValues =
    { account: ""
    , passwd: ""
    , note: ""
    }

  innerComponent :: H.Component (FormQuery q) FormContext FormOutput m
  innerComponent =
    H.mkComponent
      { initialState: identity
      , render
      , eval:
          H.mkEval
            $ H.defaultEval
                { receive = Just <<< Receive
                , handleAction = handleAction
                , handleQuery = handleQuery
                }
      }

  render :: State -> H.ComponentHTML _ _ _
  render state =
    HH.main_
      [ HH.slot _headerNav unit Nav.component unit absurd
      , HH.section
          [ classes [ "section" ] ]
          [ formArea state ]
      ]

  formArea :: FormContext -> H.ComponentHTML _ _ _
  formArea { fields, actions, formActions } =
    HH.div
      [ classes [ "container" ] ]
      [ inputSingleLineText AccountInput fields.account actions.account.handleChange
      , inputSingleLineText Passwdinput fields.passwd actions.passwd.handleChange
      , inputNote NoteTextarea fields.note actions.note.handleChange
      , HH.div
          [ classes [ "field", "is-grouped" ] ]
          [ HH.span
              [ classes [ "control" ] ]
              [ HH.button
                  [ classes [ "button", "is-dark" ]
                  , HE.onClick \_ -> formActions.submit
                  ]
                  [ HH.text "Save" ]
              ]
          , HH.span
              [ classes [ "control" ] ]
              [ HH.a
                  [ classes [ "button" ]
                  , HP.href $ hashStr List
                  ]
                  [ HH.text "Cancel" ]
              ]
          ]
      ]

  inputSingleLineText ::
    FieldType ->
    { result :: Maybe (Either ErrorCode String), value :: String | _ } ->
    (_ -> Action) ->
    H.ComponentHTML _ _ _
  inputSingleLineText ftype inp handleChange =
    let
      errArr = case inp.result of
        Just (Left e) -> [ errorMsg e ]
        _ -> []
    in
      inputFormContainer ftype (fieldLabelText ftype) errArr
        $ inputTextForm
            ftype
            (not $ null errArr)
            true
            inp.value
            handleChange

  inputNote ::
    FieldType ->
    { result :: Maybe (Either ErrorCode String), value :: String | _ } ->
    (_ -> Action) ->
    H.ComponentHTML _ _ _
  inputNote ftype inp handleChange =
    let
      errArr = case inp.result of
        Just (Left e) -> [ errorMsg e ]
        _ -> []
    in
      inputFormContainer ftype (fieldLabelText ftype) errArr
        $ textAreaForm
            ftype
            (not $ null errArr)
            inp.value
            handleChange

  handleAction :: Action -> H.HalogenM _ _ _ _ _ Unit
  handleAction = case _ of
    Receive context -> H.put context
    Eval action -> F.eval action

  handleQuery :: forall a. FormQuery q a -> H.HalogenM _ _ _ _ _ (Maybe a)
  handleQuery =
    F.handleSubmitValidate handleSuccess F.validate
      { account: isNonEmpty >=> isUnder100
      , passwd: isNonEmpty >=> isUnder100
      , note: isUnder1000
      }

  handleSuccess :: { | FieldOutput } -> H.HalogenM _ _ _ _ _ Unit
  handleSuccess out = do
    H.liftEffect $ setHash $ hashStr List
    F.raise out

  chk :: forall e v. e -> (v -> Boolean) -> v -> Either e v
  chk e r v = if r v then pure v else Left e

  isNonEmpty = chk ValueMissing $ not Str.null

  isUnder100 = chk OutOfRange $ (_ <= 100) <<< Str.length

  isUnder1000 = chk OutOfRange $ (_ <= 1000) <<< Str.length

data ErrorCode
  = OutOfRange
  | ValueMissing

derive instance genericErrorReason :: Generic ErrorCode _

instance showErrorReason :: Show ErrorCode where
  show = genericShow

errorMsg :: ErrorCode -> String
errorMsg = case _ of
  OutOfRange -> "長過ぎます"
  ValueMissing -> "入力してください"

data FieldType
  = AccountInput
  | Passwdinput
  | NoteTextarea

derive instance genericFieldType :: Generic FieldType _

instance showFieldType :: Show FieldType where
  show = genericShow

fieldLabelText :: FieldType -> String
fieldLabelText = case _ of
  AccountInput -> "Title"
  Passwdinput -> "The Work"
  NoteTextarea -> "Description"

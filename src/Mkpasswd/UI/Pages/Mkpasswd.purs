module Mkpasswd.UI.Pages.Mkpasswd where

import Prelude
import Data.Array (catMaybes, uncons)
import Data.Char.Gen (genDigitChar, genAlphaLowercase, genAlphaUppercase)
import Data.Either (Either(..), note)
import Data.Foldable (or, sum)
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.Int (toNumber, fromString)
import Data.Maybe (Maybe(..), fromMaybe, isJust)
import Data.NonEmpty (NonEmpty, (:|))
import Data.Newtype (class Newtype)
import Data.Symbol (SProxy(..))
import Data.Switch (toSwitch)
import Data.Switch as Switch
import Data.Tuple (Tuple(..))
import DOM.HTML.Indexed.StepValue (StepValue(..))
import Effect.Aff.Class (class MonadAff)
import Formless as F
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Mkpasswd (mkpasswd)
import Data.PasswdPolicy (PasswdPolicy)
import Mkpasswd.UI.Components.HeaderNav as Nav
import Mkpasswd.UI.Components.MultiChkboxes as MultiChkboxes
import Mkpasswd.UI.Routing (RouteHash(..), routeHref)
import Test.QuickCheck.Gen (Gen, elements)

type Slot id
  = forall q. H.Slot q (Maybe String) id

type ChildSlots
  = ( headerNav :: Nav.Slot Unit
    , formless :: FormSlot Unit
    )

_headerNav = SProxy :: SProxy "headerNav"

type State
  = { passwd :: Maybe String
    , errMsg :: Array String
    }

data Action
  = Generate
  | Clear
  | Save

component :: forall q i m. MonadAff m => H.Component HH.HTML q i (Maybe String) m
component =
  H.mkComponent
    { initialState
    , render
    , eval:
        H.mkEval
          $ H.defaultEval
              { handleAction = handleAction }
    }
  where
  initialState :: i -> State
  initialState =
    const
      { passwd: Nothing
      , errMsg: []
      }

  render :: State -> H.ComponentHTML Action ChildSlots m
  render state =
    HH.main
      [ HP.classes $ HH.ClassName <$> [ if isJust state.passwd then "is-clipped" else "" ] ]
      [ HH.slot _headerNav unit Nav.component unit absurd
      , HH.section
          [ HP.classes $ HH.ClassName <$> [ "section" ] ]
          [ HH.slot F._formless unit (F.component (const formInput) spec) unit (const Nothing) ]
      , footerBtnArea
      , resultModal state.passwd
      ]

  footerBtnArea :: forall slot. HH.HTML slot Action
  footerBtnArea =
    HH.section
      [ HP.classes $ HH.ClassName <$> [ "level", "sticky-bottom", "box" ] ]
      [ HH.div
          [ HP.classes $ HH.ClassName <$> [ "level-item" ] ]
          [ HH.div
              [ HP.classes $ HH.ClassName <$> [ "field", "is-grouped" ] ]
              [ HH.span
                  [ HP.classes $ HH.ClassName <$> [ "control" ] ]
                  [ HH.button
                      [ HP.classes $ HH.ClassName <$> [ "button", "is-dark" ]
                      , HE.onClick \_ -> Just Generate
                      ]
                      [ HH.text "Generate" ]
                  ]
              ]
          ]
      ]

  resultModal :: forall slot. Maybe String -> HH.HTML slot Action
  resultModal mPasswd =
    HH.div
      [ HP.classes $ HH.ClassName <$> [ "modal", if isJust mPasswd then "is-active" else "" ] ]
      [ HH.div
          [ HP.classes $ HH.ClassName <$> [ "modal-background" ]
          , HE.onClick \_ -> Just Clear
          ]
          []
      , HH.div
          [ HP.classes $ HH.ClassName <$> [ "modal-card" ] ]
          [ HH.header
              [ HP.classes $ HH.ClassName <$> [ "modal-card-head" ] ]
              [ HH.h1
                  [ HP.classes $ HH.ClassName <$> [ "modal-card-title" ] ]
                  [ HH.text "Result" ]
              , HH.button
                  [ HP.classes $ HH.ClassName <$> [ "delete" ]
                  , HP.attr (HH.AttrName "aria-label") "close"
                  , HE.onClick \_ -> Just Clear
                  ]
                  []
              ]
          , HH.section
              [ HP.classes $ HH.ClassName <$> [ "modal-card-body" ] ]
              [ HH.div
                  [ HP.classes $ HH.ClassName <$> [ "level" ] ]
                  [ HH.div
                      [ HP.classes $ HH.ClassName <$> [ "level-item", "has-text-centered", "is-size-3", "text-wrap" ] ]
                      [ HH.br_
                      , HH.text $ fromMaybe "" mPasswd
                      , HH.br_
                      ]
                  ]
              ]
          , HH.footer
              [ HP.classes $ HH.ClassName <$> [ "modal-card-foot" ] ]
              [ HH.a
                  [ HP.classes $ HH.ClassName <$> [ "button", "is-dark" ]
                  , HE.onClick \_ -> Just Save
                  , HP.href $ routeHref New
                  ]
                  [ HH.text "Save" ]
              , HH.button
                  [ HP.classes $ HH.ClassName <$> [ "button" ]
                  , HE.onClick \_ -> Just Generate
                  ]
                  [ HH.text "Regenerate" ]
              ]
          ]
      ]

  handleAction :: Action -> H.HalogenM State Action ChildSlots (Maybe String) m Unit
  handleAction = case _ of
    Clear -> do
      H.modify_ _ { passwd = Nothing }
    Generate -> do
      mRes <- H.query F._formless unit $ F.injQuery (H.request GetPasswdPolicy)
      case mRes of
        Just policy -> do
          newPasswd <- H.liftEffect $ mkpasswd policy
          H.modify_ _ { passwd = newPasswd }
        Nothing -> pure unit
    Save -> do
      H.raise =<< H.gets _.passwd

-- Formless
type GenPasswdConfig
  = { length :: Int
    , isNonEmpty :: Boolean
    , numDigit :: Int
    , isUseDigit :: Boolean
    , numLower :: Int
    , isUseLower :: Boolean
    , numUpper :: Int
    , isUseUpper :: Boolean
    , numSymbol :: Int
    , isUseSymbol :: Boolean
    }

newtype Form r f
  = Form
  ( r
      ( length :: f (Array ErrorCode) String Int
      , isNonEmpty :: f (Array ErrorCode) Boolean Boolean
      , numDigit :: f (Array ErrorCode) String Int
      , isUseDigit :: f (Array ErrorCode) Boolean Boolean
      , numLower :: f (Array ErrorCode) String Int
      , isUseLower :: f (Array ErrorCode) Boolean Boolean
      , numUpper :: f (Array ErrorCode) String Int
      , isUseUpper :: f (Array ErrorCode) Boolean Boolean
      , numSymbol :: f (Array ErrorCode) String Int
      , isUseSymbol :: f (Array ErrorCode) Boolean Boolean
      )
  )

derive instance newtypeForm :: Newtype (Form r f) _

_length = SProxy :: SProxy "length"

_isNonEmpty = SProxy :: SProxy "isNonEmpty"

_numDigit = SProxy :: SProxy "numDigit"

_isUseDigit = SProxy :: SProxy "isUseDigit"

_numUpper = SProxy :: SProxy "numUpper"

_isUseUpper = SProxy :: SProxy "isUseUpper"

_numLower = SProxy :: SProxy "numLower"

_isUseLower = SProxy :: SProxy "isUseLower"

_numSymbol = SProxy :: SProxy "numSymbol"

_isUseSymbol = SProxy :: SProxy "isUseSymbol"

data ErrorCode
  = OutOfRange
  | ValueMissing
  | TypeMismatch
  | EmptyCharSet
  | TooShort

derive instance genericErrorReason :: Generic ErrorCode _

instance showErrorReason :: Show ErrorCode where
  show = genericShow

maxLength :: Int
maxLength = 100

minLength :: Int
minLength = 0

formInput :: forall m. Monad m => F.Input' Form m
formInput =
  { initialInputs:
      Just
        $ F.wrapInputFields
            { length: "9"
            , isNonEmpty: true
            , numDigit: "2"
            , isUseDigit: true
            , numUpper: "2"
            , isUseUpper: true
            , numLower: "2"
            , isUseLower: true
            , numSymbol: "1"
            , isUseSymbol: true
            }
  , validators:
      Form
        { length: isInt >>> isInRange >>> enoughLong
        , isNonEmpty: atLeastOneTrue
        , numDigit: isInt >>> isInRange
        , isUseDigit: F.noValidation
        , numUpper: isInt >>> isInRange
        , isUseUpper: F.noValidation
        , numLower: isInt >>> isInRange
        , isUseLower: F.noValidation
        , numSymbol: isInt >>> isInRange
        , isUseSymbol: F.noValidation
        }
  }
  where
  chk :: forall e v. e -> (v -> Boolean) -> v -> Either (Array e) v
  chk e r v = if r v then pure v else Left [ e ]

  isInt = F.hoistFnE_ (note [ TypeMismatch ] <<< fromString)

  isInRange = F.hoistFnE_ (chk OutOfRange $ between minLength maxLength)

  atLeastOneTrue =
    F.hoistFnE \form _ ->
      chk EmptyCharSet identity
        $ or
            [ F.getInput _isUseDigit form
            , F.getInput _isUseUpper form
            , F.getInput _isUseLower form
            , F.getInput _isUseSymbol form
            ]

  enoughLong =
    F.hoistFnE \form len ->
      let
        requiredLength =
          sum
            $ catMaybes
                [ F.getOutput _numDigit form
                , F.getOutput _numUpper form
                , F.getOutput _numLower form
                , F.getOutput _numSymbol form
                ]
      in
        chk TooShort (_ >= requiredLength) len

data FieldType
  = PasswdLength
  | DigitCharNum
  | AlphaUppercase
  | AlphaLowercaseNum
  | SymbolCharNum

derive instance genericFieldType :: Generic FieldType _

instance showFieldType :: Show FieldType where
  show = genericShow

data PasswdConfigAction
  = ToggleCharSetUse FieldType Boolean

data PasswdConfigQuery a
  = GetPasswdPolicy (PasswdPolicy Gen -> a)

derive instance functorPasswdConfigQuery :: Functor PasswdConfigQuery

type FormAction
  = F.Action Form PasswdConfigAction

type FormSlot
  = F.Slot Form PasswdConfigQuery GrandchildSlots GenPasswdConfig

type GrandchildSlots
  = ( multichkboxes :: MultiChkboxes.Slot Unit )

_multichkboxes = SProxy :: SProxy "multichkboxes"

spec :: forall i m. MonadAff m => F.Spec Form () PasswdConfigQuery PasswdConfigAction GrandchildSlots i GenPasswdConfig m
spec =
  F.defaultSpec
    { render = render
    , handleAction = handleAction
    , handleQuery = handleQuery
    , handleEvent = F.raiseResult
    }
  where
  eval act = F.handleAction handleAction F.raiseResult act

  handleQuery :: forall a. PasswdConfigQuery a -> F.HalogenM Form () PasswdConfigAction GrandchildSlots GenPasswdConfig m (Maybe a)
  handleQuery = case _ of
    GetPasswdPolicy reply -> do
      eval F.validateAll
      form <- H.gets _.form
      let
        conf =
          { length: _, isNonEmpty: _, numDigit: _, isUseDigit: _, numLower: _, isUseLower: _, numUpper: _, isUseUpper: _, numSymbol: _, isUseSymbol: _ }
            <$> (F.getOutput _length form)
            <*> (F.getOutput _isNonEmpty form)
            <*> (F.getOutput _numDigit form)
            <*> (F.getOutput _isUseDigit form)
            <*> (F.getOutput _numUpper form)
            <*> (F.getOutput _isUseUpper form)
            <*> (F.getOutput _numLower form)
            <*> (F.getOutput _isUseLower form)
            <*> (F.getOutput _numSymbol form)
            <*> (F.getOutput _isUseSymbol form)
      mCharSet <- H.query _multichkboxes unit $ H.request F.submitReply
      let
        mAllowedSymbols = (_.allowedSymbols <<< F.unwrapOutputFields) <$> (join mCharSet)
      let
        policy = join $ toPaswdPolicy <$> conf <*> mAllowedSymbols
      pure $ reply <$> policy

  toPaswdPolicy :: GenPasswdConfig -> NonEmpty Array Char -> Maybe (PasswdPolicy Gen)
  toPaswdPolicy conf allowedSymbols =
    let
      seed =
        (uncons <<< catMaybes) $ Switch.toMaybe
          <$> [ toSwitch conf.isUseDigit (Tuple conf.numDigit genDigitChar)
            , toSwitch conf.isUseUpper (Tuple conf.numUpper genAlphaUppercase)
            , toSwitch conf.isUseLower (Tuple conf.numLower genAlphaLowercase)
            , toSwitch conf.isUseSymbol (Tuple conf.numSymbol $ elements allowedSymbols)
            ]
    in
      { length: conf.length, required: _ } <<< (\{ head, tail } -> head :| tail) <$> seed

  handleAction :: PasswdConfigAction -> F.HalogenM Form () PasswdConfigAction GrandchildSlots GenPasswdConfig m Unit
  handleAction = case _ of
    ToggleCharSetUse PasswdLength _ -> pure unit
    ToggleCharSetUse DigitCharNum flg -> do
      eval $ F.set _isUseDigit flg
      eval $ F.setValidate _isNonEmpty flg
    ToggleCharSetUse AlphaUppercase flg -> do
      eval $ F.set _isUseUpper flg
      eval $ F.setValidate _isNonEmpty flg
    ToggleCharSetUse AlphaLowercaseNum flg -> do
      eval $ F.set _isUseLower flg
      eval $ F.setValidate _isNonEmpty flg
    ToggleCharSetUse SymbolCharNum flg -> do
      eval $ F.set _isUseSymbol flg
      eval $ F.setValidate _isNonEmpty flg

  errorMsg :: ErrorCode -> String
  errorMsg = case _ of
    OutOfRange -> (show minLength) <> "と" <> (show maxLength) <> "の間で入力してください"
    ValueMissing -> "入力してください"
    TypeMismatch -> "整数を入力してください"
    EmptyCharSet -> "使用する文字を指定してください"
    TooShort -> "長さは文字種ごとの必要最低数の総和よりも大きくしてください"

  render :: forall st. F.PublicState Form st -> F.ComponentHTML Form PasswdConfigAction GrandchildSlots m
  render fstate =
    HH.div
      [ HP.classes $ HH.ClassName <$> [ "container" ] ]
      [ let
          err = F.getError _length fstate.form
        in
          HH.div
            [ HP.classes $ HH.ClassName <$> [ "field" ] ]
            [ labelBlock PasswdLength
            , inputNumberForm PasswdLength (isJust err) true (F.getInput _length fstate.form) (Just <<< F.setValidate _length)
            , errorDisplay err
            ]
      , inputFormGroup
          DigitCharNum
          (F.getError _numDigit fstate.form)
          (F.getInput _isUseDigit fstate.form)
          (F.getInput _numDigit fstate.form)
          (Just <<< F.injAction <<< ToggleCharSetUse DigitCharNum)
          (Just <<< F.setValidate _numDigit)
      , inputFormGroup
          AlphaUppercase
          (F.getError _numUpper fstate.form)
          (F.getInput _isUseUpper fstate.form)
          (F.getInput _numUpper fstate.form)
          (Just <<< F.injAction <<< ToggleCharSetUse AlphaUppercase)
          (Just <<< F.setValidate _numUpper)
      , inputFormGroup
          AlphaLowercaseNum
          (F.getError _numLower fstate.form)
          (F.getInput _isUseLower fstate.form)
          (F.getInput _numLower fstate.form)
          (Just <<< F.injAction <<< ToggleCharSetUse AlphaLowercaseNum)
          (Just <<< F.setValidate _numLower)
      , inputFormGroup
          SymbolCharNum
          (F.getError _numSymbol fstate.form)
          (F.getInput _isUseSymbol fstate.form)
          (F.getInput _numSymbol fstate.form)
          (Just <<< F.injAction <<< ToggleCharSetUse SymbolCharNum)
          (Just <<< F.setValidate _numSymbol)
      , errorDisplay (F.getError _isNonEmpty fstate.form)
      , HH.slot _multichkboxes unit MultiChkboxes.component (not $ F.getInput _isUseSymbol fstate.form) (const Nothing)
      ]

  inputFormGroup ::
    forall slot.
    FieldType ->
    Maybe (Array ErrorCode) ->
    Boolean ->
    String ->
    (Boolean -> Maybe FormAction) ->
    (String -> Maybe FormAction) ->
    HH.HTML slot FormAction
  inputFormGroup fieldType err isUsed inp onChecked onInput =
    HH.div
      [ HP.classes $ HH.ClassName <$> [ "field" ] ]
      [ labelBlock fieldType
      , HH.div
          [ HP.classes $ HH.ClassName <$> [ "field", "has-addons" ] ]
          [ inputAddon isUsed onChecked
          , inputNumberForm fieldType (isJust err) isUsed inp onInput
          ]
      , errorDisplay err
      ]

  fieldLabelText :: FieldType -> String
  fieldLabelText = case _ of
    PasswdLength -> "Length"
    DigitCharNum -> "Numeral"
    AlphaUppercase -> "Uppercase Alphabet"
    AlphaLowercaseNum -> "Lowercase Alphabet"
    SymbolCharNum -> "Symbol"

  labelBlock :: forall slot. FieldType -> HH.HTML slot FormAction
  labelBlock fieldType =
    HH.label
      [ HP.for $ show fieldType
      , HP.classes $ HH.ClassName <$> [ "label" ]
      ]
      [ HH.text (fieldLabelText fieldType) ]

  inputNumberForm ::
    forall slot.
    FieldType ->
    Boolean ->
    Boolean ->
    String ->
    (String -> Maybe FormAction) ->
    HH.HTML slot FormAction
  inputNumberForm fieldType hasErr isUsed inp onInput =
    HH.div
      [ HP.classes $ HH.ClassName <$> [ "control", "is-expanded" ] ]
      [ HH.input
          [ HP.type_ HP.InputNumber
          , HP.id_ $ show fieldType
          , HP.classes $ HH.ClassName
              <$> [ "input"
                , if hasErr then
                    "is-danger"
                  else
                    ""
                ]
          , HP.value inp
          , HE.onValueInput onInput
          , HP.step Any
          , HP.max (toNumber maxLength)
          , HP.min (toNumber minLength)
          , HP.disabled (not isUsed)
          ]
      ]

  inputAddon :: forall slot. Boolean -> (Boolean -> Maybe FormAction) -> HH.HTML slot FormAction
  inputAddon isUsed onChecked =
    HH.span
      [ HP.classes $ HH.ClassName <$> [ "control" ] ]
      [ HH.label
          [ HP.classes $ HH.ClassName <$> [ "button", "checkbox" ]
          ]
          [ HH.input
              [ HP.type_ HP.InputCheckbox
              , HP.checked isUsed
              , HP.classes $ HH.ClassName <$> [ "mr1" ]
              , HE.onChecked onChecked
              ]
          ]
      ]

  errorDisplay :: forall slot. Maybe (Array ErrorCode) -> HH.HTML slot FormAction
  errorDisplay err =
    HH.ul
      [ HP.classes $ HH.ClassName <$> [ "help", "is-danger" ] ]
      $ (\msg -> HH.li_ [ HH.text (errorMsg msg) ])
      <$> (fromMaybe [] $ err)

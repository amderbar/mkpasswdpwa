module Page.Mkpasswd
  ( Slot
  , component
  ) where

import Prelude

import Component.HeaderNav as Nav
import Component.RenderUtil (classes, footerBtnArea, inputAddon, inputFormContainer, inputNumberForm, inputTextForm, resultModal)
import Control.Monad.Error.Class (liftMaybe)
import Data.Array (catMaybes, null, singleton)
import Data.Array.NonEmpty (NonEmptyArray, fromArray, toArray)
import Data.Char.Subset (SymbolChar, symbols, toString)
import Data.Char.Subset (fromString) as SubsetChar
import Data.Count (Count, toCountE)
import Data.Either (Either(..), note)
import Data.Generic.Rep (class Generic)
import Data.Int (fromString) as Int
import Data.Length (Length, toLengthE)
import Data.Maybe (Maybe(..), isJust)
import Data.Newtype (unwrap)
import Data.Passwd (Passwd)
import Data.Passwd.Gen (genPasswd)
import Data.Policy (CharGenSrc(..), Policy, mkPolicy)
import Data.Show.Generic (genericShow)
import Data.Switch (toMaybe, toSwitch)
import Effect.Aff.Class (class MonadAff)
import Effect.Console (logShow) as Console
import Formless as F
import Halogen as H
import Halogen.HTML as HH
import Test.QuickCheck.Gen (Gen, randomSampleOne)
import Type.Proxy (Proxy(..))

type Slot id
  = forall q. H.Slot q Void id

type ChildSlots
  = ( headerNav :: Nav.Slot Unit )

_headerNav = Proxy :: Proxy "headerNav"

type Form (f :: Type -> Type -> Type -> Type)
  = ( length :: f String ErrorCode Length
    , digitIsOn :: f Boolean Void Boolean
    , digitCount :: f String ErrorCode Count
    , capitalIsOn :: f Boolean Void Boolean
    , capitalCount :: f String ErrorCode Count
    , lowercaseIsOn :: f Boolean Void Boolean
    , lowercaseCount :: f String ErrorCode Count
    , symbolIsOn :: f Boolean Void Boolean
    , symbolCount :: f String ErrorCode Count
    , symbolSet :: f String ErrorCode (NonEmptyArray SymbolChar)
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
  = { formCtx :: FormContext
    , passwd :: Maybe Passwd
    , errMsg :: Array String
    }

type Input
  = Unit

type Output
  = Maybe Passwd

data Action
  = Receive FormContext
  | Eval (F.FormlessAction FieldState)
  | Generate
  | Clear
  | Save

component :: forall q m. MonadAff m => H.Component q Input Output m
component =
  (\c -> F.formless c initialValues innerComponent)
    { liftAction: Eval
    , validateOnMount: true
    , validateOnChange: true
    }
  where
  initialValues :: { | Form F.FieldInput }
  initialValues =
    { length: show 9
    , digitIsOn: true
    , digitCount: show 2
    , capitalIsOn: true
    , capitalCount: show 2
    , lowercaseIsOn: true
    , lowercaseCount: show 2
    , symbolIsOn: true
    , symbolCount: show 1
    , symbolSet: toString $ toArray symbols
    }

  innerComponent :: H.Component (FormQuery q) FormContext FormOutput m
  innerComponent =
    H.mkComponent
      { initialState: { formCtx: _, passwd: Nothing, errMsg: [] }
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
    HH.main
      [ classes [ if isJust state.passwd then "is-clipped" else "" ] ]
      [ HH.slot _headerNav unit Nav.component unit absurd
      , formArea state.formCtx
      , footerBtnArea (const Generate)
      , resultModal
          { onclickClose: const Clear
          , onclickSave: const Save
          , onclickRegenerate: const Generate
          }
          (unwrap <$> state.passwd)
      ]

  formArea :: FormContext -> H.ComponentHTML _ _ _
  formArea { fields, actions } =
    HH.section
      [ classes [ "section" ] ]
      [ HH.div
          [ classes [ "container" ] ]
          [ inputLength PasswdLength fields.length actions.length.handleChange
          , inputCountSwitch
              DigitCharNum
              { isOn: fields.digitIsOn, count: fields.digitCount }
              { isOn: actions.digitIsOn.handleChange, count: actions.digitCount.handleChange }
          , inputCountSwitch
              AlphaUppercaseNum
              { isOn: fields.capitalIsOn, count: fields.capitalCount }
              { isOn: actions.capitalIsOn.handleChange, count: actions.capitalCount.handleChange }
          , inputCountSwitch
              AlphaLowercaseNum
              { isOn: fields.lowercaseIsOn, count: fields.lowercaseCount }
              { isOn: actions.lowercaseIsOn.handleChange, count: actions.lowercaseCount.handleChange }
          , inputCountSwitch
              SymbolCharNum
              { isOn: fields.symbolIsOn, count: fields.symbolCount }
              { isOn: actions.symbolIsOn.handleChange, count: actions.symbolCount.handleChange }
          , inputSymbolCharSet SymbolCharSet fields.symbolSet actions.symbolSet.handleChange
          ]
      ]

  inputLength ::
    FieldType ->
    { result :: Maybe (Either ErrorCode Length), value :: String | _ } ->
    (_ -> Action) ->
    H.ComponentHTML _ _ _
  inputLength ftype length handleChange =
    let
      errArr = case length.result of
        Just (Left e) -> [ errorMsg e ]
        _ -> []
    in
      inputFormContainer ftype (fieldLabelText ftype) errArr
        $ inputNumberForm
            ftype
            (not $ null errArr)
            true
            length.value
            handleChange

  inputCountSwitch ::
    FieldType ->
    { isOn :: { value :: Boolean | _ }, count :: { result :: Maybe (Either ErrorCode Count), value :: String | _ } } ->
    { isOn :: _ -> Action, count :: _ -> Action } ->
    H.ComponentHTML _ _ _
  inputCountSwitch ftype { isOn, count } handleChange =
    let
      errArr = case count.result of
        Just (Left e) -> [ errorMsg e ]
        _ -> []
    in
      inputFormContainer ftype (fieldLabelText ftype) errArr
        $ HH.div
            [ classes [ "field", "has-addons" ] ]
            [ inputAddon
                isOn.value
                handleChange.isOn
            , inputNumberForm
                ftype
                (not $ null errArr)
                isOn.value
                count.value
                handleChange.count
            ]

  inputSymbolCharSet ftype charset handleChange =
    let
      errArr = case charset.result of
        Just (Left e) -> [ errorMsg e ]
        _ -> []
    in
      inputFormContainer ftype (fieldLabelText ftype) errArr
        $ inputTextForm
            ftype
            (not $ null errArr)
            true
            charset.value
            handleChange

  handleAction :: Action -> H.HalogenM _ _ _ _ _ Unit
  handleAction = case _ of
    Receive context -> H.modify_ _ { formCtx = context }
    Eval action -> F.eval action
    Clear -> H.modify_ _ { passwd = Nothing }
    Generate -> do
      inp <- H.gets _.formCtx.fields
      case decodeGen inp of
        Right gen -> do
          newPasswd <- H.liftEffect $ randomSampleOne gen
          H.modify_ _ { passwd = Just newPasswd }
        Left err -> H.liftEffect $ Console.logShow err
    Save -> F.raise =<< H.gets _.passwd

  handleQuery :: forall a. FormQuery q a -> H.HalogenM _ _ _ _ _ (Maybe a)
  handleQuery =
    F.handleSubmitValidate handleSuccess F.validate
      { length: validateLength
      , digitIsOn: Right
      , digitCount: validateCount
      , lowercaseIsOn: Right
      , lowercaseCount: validateCount
      , capitalIsOn: Right
      , capitalCount: validateCount
      , symbolIsOn: Right
      , symbolCount: validateCount
      , symbolSet: validateSymbolCharSet
      }
    where
    handleSuccess :: { | FieldOutput } -> H.HalogenM _ _ _ _ _ Unit
    handleSuccess = void <<< pure

decodeGen :: { | FieldState } -> Either (Array ErrorCode) (Gen Passwd)
decodeGen inp = genPasswd <$> decodeFormInput inp

decodeFormInput :: { | FieldState } -> Either (Array ErrorCode) Policy
decodeFormInput f = do
  length <- joinResult singleton f.length.result
  digitIsOn <- joinResult (\_ -> []) f.digitIsOn.result
  digitCount <- joinResult singleton f.digitCount.result
  lowercaseIsOn <- joinResult (\_ -> []) f.lowercaseIsOn.result
  lowercaseCount <- joinResult singleton f.lowercaseCount.result
  capitalIsOn <- joinResult (\_ -> []) f.capitalIsOn.result
  capitalCount <- joinResult singleton f.capitalCount.result
  symbolIsOn <- joinResult (\_ -> []) f.symbolIsOn.result
  symbolCount <- joinResult singleton f.symbolCount.result
  symbolSet <- joinResult singleton f.symbolSet.result
  let confs =
        catMaybes
          [ meybeIf digitIsOn digitCount <#> { count: _, genSrc: Digits }
          , meybeIf capitalIsOn capitalCount <#> { count: _, genSrc: UppercaseAlphabets }
          , meybeIf lowercaseIsOn lowercaseCount <#> { count: _, genSrc: LowercaseAlphabets }
          , meybeIf symbolIsOn symbolCount <#> { count: _, genSrc: Symbols symbolSet }
          ]
  joinResult singleton $ Just (note EmptyCharSet $ mkPolicy length confs)
  where
  meybeIf p = toMaybe <<< toSwitch p

mapLeft :: forall e e' r. (e -> e') -> Either e r -> Either e' r
mapLeft f = case _ of
  Right r -> Right r
  Left e -> Left (f e)

joinResult :: forall e e' a. (e -> Array e') -> Maybe (Either e a) -> Either (Array e') a
joinResult f = case _ of
  Just e -> mapLeft f e
  Nothing -> Left []

fromStringE :: String -> Either ErrorCode Int
fromStringE = Int.fromString >>> note TypeMismatch

validateLength :: String -> Either ErrorCode Length
validateLength = fromStringE >=> (toLengthE OutOfRange)

validateCount :: String -> Either ErrorCode Count
validateCount = fromStringE >=> (toCountE OutOfRange)

validateSymbolCharSet :: String -> Either ErrorCode (NonEmptyArray SymbolChar)
validateSymbolCharSet = SubsetChar.fromString (const TypeMismatch) >=> (liftMaybe ValueMissing <<< fromArray)

data FieldType
  = PasswdLength
  | DigitCharNum
  | AlphaUppercaseNum
  | AlphaLowercaseNum
  | SymbolCharNum
  | SymbolCharSet

derive instance eqFieldType :: Eq FieldType

derive instance ordFieldType :: Ord FieldType

derive instance genericFieldType :: Generic FieldType _

instance showFieldType :: Show FieldType where
  show = genericShow

fieldLabelText :: FieldType -> String
fieldLabelText = case _ of
  PasswdLength -> "Length"
  DigitCharNum -> "Numeral"
  AlphaUppercaseNum -> "Uppercase Alphabet"
  AlphaLowercaseNum -> "Lowercase Alphabet"
  SymbolCharNum -> "Symbol"
  SymbolCharSet -> "Available Symbols"

data ErrorCode
  = OutOfRange Int Int
  | ValueMissing
  | TypeMismatch
  | EmptyCharSet
  | TooShort

derive instance genericErrorReason :: Generic ErrorCode _

instance showErrorReason :: Show ErrorCode where
  show = genericShow

errorMsg :: ErrorCode -> String
errorMsg = case _ of
  OutOfRange b t -> (show b) <> "と" <> (show t) <> "の間で入力してください"
  ValueMissing -> "入力してください"
  TypeMismatch -> "整数を入力してください"
  EmptyCharSet -> "使用する文字を指定してください"
  TooShort -> "長さは文字種ごとの必要最低数の総和よりも大きくしてください"

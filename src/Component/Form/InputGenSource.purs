module Component.Form.InputGenSource
  ( ErrMsg
  , Input
  , Query(..)
  , Result
  , Slot
  , defaultInput
  , proxy
  , slot
  , slot_
  )
  where

import Prelude

import Component.RenderUtil (classes)
import Control.Monad.Error.Class (liftMaybe)
import Data.Array.NonEmpty (NonEmptyArray)
import Data.Char.Subset (class SubsetChar, toString)
import Data.Char.Subset as SubsetChar
import Data.Either (Either(..), isLeft)
import Data.GenSource (members)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Policy (CharGenSrc(..))
import Data.String.NonEmpty (fromString)
import Effect.Aff.Class (class MonadAff)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Record.Builder as R
import Type.Proxy (Proxy(..))

type Slot slot row =
  ( "InputGenSource" :: H.Slot Query Result slot | row )

proxy = Proxy :: Proxy "InputGenSource"

slot ::
  forall (action :: Type) (m :: Type -> Type) (slot :: Type) (row :: Row Type).
  Ord slot =>
  MonadAff m =>
  slot ->
  {|Input} ->
  (Result -> action) ->
  H.ComponentHTML action (Slot slot row) m
slot id input = HH.slot proxy id component input

slot_ ::
  forall (action :: Type) (m :: Type -> Type) (slot :: Type) (row :: Row Type).
  Ord slot =>
  MonadAff m =>
  slot ->
  {|Input} ->
  H.ComponentHTML action (Slot slot row) m
slot_ id input = HH.slot_ proxy id component input

isCustamizable :: forall c. CharGenSrc c -> Boolean
isCustamizable = case _ of
  Digits -> false
  UppercaseAlphabets -> false
  LowercaseAlphabets -> false
  Symbols _ -> true
  Hiraganas _ -> true
  AnyChars _ -> true

getValue :: CharGenSrc Array -> String
getValue = case _ of
  Digits -> membersStr Digits
  UppercaseAlphabets -> membersStr UppercaseAlphabets
  LowercaseAlphabets -> membersStr LowercaseAlphabets
  Symbols cs -> toString cs
  Hiraganas cs -> toString cs
  AnyChars cs -> toString cs
  where
    membersStr :: CharGenSrc NonEmptyArray -> String
    membersStr = toString <<< members

type Input =
  ( id :: Maybe String
  , disabled :: Boolean
  , genSrc :: CharGenSrc Array
  )

defaultInput :: {|Input}
defaultInput =
  { id: Nothing
  , disabled: false
  , genSrc: Digits
  }

data Query a = Validate (Result -> a)

type ErrMsg = String

type Result = Either ErrMsg (CharGenSrc NonEmptyArray)

type State =
  { value :: String
  , hasErr :: Boolean
  | Input
  }

data Action
  = InputValue String

component :: forall m. MonadAff m => H.Component Query {|Input} Result m
component =
  H.mkComponent
    { initialState: \inp@{genSrc} ->
      let value = getValue genSrc
      in R.build (R.merge { value, hasErr: false }) inp
    , render
    , eval:
      H.mkEval $
        H.defaultEval
          { handleAction = handleAction
          , handleQuery = handleQuery
          }
    }
  where
  render :: State -> H.ComponentHTML _ _ _
  render {id, disabled, genSrc, value, hasErr} =
    HH.div
      [ classes [ "control", "is-expanded" ] ]
      [ HH.input $
          [ HP.type_ HP.InputText
          , classes
            [ "input"
            , if hasErr then "is-danger" else ""
            , if not (isCustamizable genSrc) then "has-text-grey" else ""
            ]
          , HP.value value
          , HE.onValueInput InputValue
          , HP.readOnly $ not (isCustamizable genSrc)
          , HP.disabled disabled
          ]
          <> fromMaybe [] (pure <<< HP.id <$> id)
      ]

  handleAction :: Action -> H.HalogenM _ _ _ _ _ Unit
  handleAction (InputValue str) = do
    {genSrc} <- H.get
    let ret = validateCharSet str genSrc
    H.modify_ _ {value = str, hasErr = isLeft ret}
    H.raise ret

  handleQuery :: forall a. Query a -> H.HalogenM _ _ _ _ _ (Maybe a)
  handleQuery (Validate reply) = do
    {genSrc, value} <- H.get
    let ret = validateCharSet value genSrc
    H.modify_ _ {hasErr = isLeft ret}
    pure (Just $ reply ret)

validateCharSet :: String -> CharGenSrc Array -> Result
validateCharSet str = case _ of
  Digits -> Right Digits
  UppercaseAlphabets -> Right UppercaseAlphabets
  LowercaseAlphabets -> Right LowercaseAlphabets
  Symbols _ -> Symbols <$> validateMembers
  Hiraganas _ -> Hiraganas <$> validateMembers
  AnyChars _ -> AnyChars <$> validateMembers
  where
  validateMembers :: forall c. SubsetChar c => Either ErrMsg (NonEmptyArray c)
  validateMembers = do
    nes <- liftMaybe "使用する文字を入力してください" (fromString str)
    SubsetChar.fromNonEmptyString (show >>> append "使用できない文字です: ") nes

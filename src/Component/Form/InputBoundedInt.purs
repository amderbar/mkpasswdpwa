module Component.Form.InputBoundedInt
  ( ErrCase(..)
  , Input
  , Query(..)
  , defultInput
  , proxy
  , slot
  )
  where

import Prelude

import Component.RenderUtil (classes)
import Control.Monad.Error.Class (class MonadError, liftEither, liftMaybe)
import Data.Either (Either, isLeft)
import Data.Int as Int
import Data.Int.Interval (class IntInterval, fromInt, toNumber)
import Data.Maybe (Maybe(..), fromMaybe)
import Effect.Aff.Class (class MonadAff)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Record.Builder as R
import Type.Proxy (Proxy(..))

type Slot slot output row =
  ( "InputBoundedInt" :: H.Slot (Query output) output slot
  | row
  )

proxy = Proxy :: Proxy "InputBoundedInt"

slot ::
  forall action m slot t error output row.
  Ord slot =>
  MonadAff m =>
  MonadError error t =>
  IntInterval output =>
  (ErrCase -> error) ->
  slot ->
  {| Input } ->
  (t output -> action) ->
  H.ComponentHTML action (Slot slot (t output) row) m
slot onErr id input = HH.slot proxy id (component onErr) input

type Input =
  ( id :: Maybe String
  , step :: HP.StepValue
  , disabled :: Boolean
  , value :: Int
  )

data Query o a = Validate (o -> a)

data ErrCase
  = OutOfRange Int Int
  | InvalidInt

defultInput :: {| Input }
defultInput =
  { id: Nothing
  , step: HP.Any
  , disabled: false
  , value: 0
  }

data Action
  = ValueInput String

type State =
  { strValue :: String
  , hasErr :: Boolean
  | Input
  }

component ::
  forall t e o m.
  MonadAff m =>
  MonadError e t =>
  IntInterval o =>
  (ErrCase -> e) ->
  H.Component (Query (t o)) {| Input } (t o) m
component onErr =
  H.mkComponent
    { initialState: \inp@{ value } -> R.build (R.merge { strValue: show value, hasErr: false }) inp
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
  render { id, step, disabled, strValue, hasErr } =
    HH.input $
      [ HP.type_ HP.InputNumber
      , classes ["input", if hasErr then "is-danger" else ""]
      , HP.value strValue
      , HE.onValueInput ValueInput
      , HP.step step
      , HP.max $ toNumber (top :: o)
      , HP.min $ toNumber (bottom :: o)
      , HP.disabled disabled
      ]
      <> fromMaybe [] (pure <<< HP.id <$> id)

  handleAction :: Action -> H.HalogenM _ _ _ _ _ Unit
  handleAction = case _ of
    ValueInput str -> do
      let ret = parseValue str
      H.modify_ _ {strValue = str, hasErr = isLeft ret}
      H.raise (liftEither ret)

  handleQuery :: forall a. Query (t o) a -> H.HalogenM _ _ _ _ _ (Maybe a)
  handleQuery (Validate reply) = do
    ret <- H.gets (_.strValue >>> parseValue)
    H.modify_ _ {hasErr = isLeft ret}
    pure (Just $ reply $ liftEither ret)

  parseValue :: String -> Either e o
  parseValue str = do
    i <- liftMaybe (onErr InvalidInt) $ Int.fromString str
    fromInt (\b t -> onErr $ OutOfRange b t) i

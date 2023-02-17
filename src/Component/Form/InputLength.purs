module Component.Form.InputLength
  ( Input
  , Output
  , Query(..)
  , proxy
  , slot
  , slot_
  )
  where

import Prelude

import Component.Form.InputBoundedInt as Base
import Component.RenderUtil (classes, errorDisplay)
import Data.Either (Either(..), hush)
import Data.Length (Length)
import Data.Maybe (Maybe(..))
import Effect.Aff.Class (class MonadAff)
import Halogen as H
import Halogen.HTML as HH
import Type.Proxy (Proxy(..))

type Slot slot row =
  ( "InputLength" :: H.Slot Query Output slot | row )

proxy = Proxy :: Proxy "InputLength"

slot ::
  forall (action :: Type) (m :: Type -> Type) (slot :: Type) (row :: Row Type).
  Ord slot =>
  MonadAff m =>
  slot ->
  Input ->
  (Output -> action) ->
  H.ComponentHTML action (Slot slot row) m
slot id input = HH.slot proxy id component input

slot_ ::
  forall (action :: Type) (m :: Type -> Type) (slot :: Type) (row :: Row Type).
  Ord slot =>
  MonadAff m =>
  slot ->
  Input ->
  H.ComponentHTML action (Slot slot row) m
slot_ id input = HH.slot_ proxy id component input

type Input = Int

type Output = Length

data Query a = GetResult (Output -> a)

type ErrMsg = String

type State =
  { input :: Input
  , errMsg :: Array ErrMsg
  }

data Action
  = Validated (Either ErrMsg Output)

component :: forall m. MonadAff m => H.Component Query Input Output m
component =
  H.mkComponent
    { initialState: {input: _, errMsg: []}
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
  render {input, errMsg} =
    HH.div
      [ classes [ "field" ] ]
      [ HH.label
        [ classes [ "label" ] ]
        [ HH.text "Length" ]
      , Validated # Base.slot errorMsg unit (Base.defultInput { value = input })
      , errorDisplay errMsg
      ]

  errorMsg :: Base.ErrCase -> ErrMsg
  errorMsg = case _ of
    Base.InvalidInt -> "整数を入力してください"
    Base.OutOfRange b t -> (show b) <> "と" <> (show t) <> "の間で入力してください"

  handleAction :: Action -> H.HalogenM _ _ _ _ _ Unit
  handleAction (Validated ret) = do
    H.modify_ _ {errMsg = errMsgArr (Just ret)}
    case ret of
      Right len -> H.raise len
      Left _ -> pure unit

  handleQuery :: forall a. Query a -> H.HalogenM _ _ _ _ _ (Maybe a)
  handleQuery (GetResult reply) = do
    ret <- H.request Base.proxy unit Base.Validate
    H.modify_ _ {errMsg = errMsgArr ret}
    pure $ reply <$> (hush =<< ret)

  errMsgArr :: Maybe (Either ErrMsg Output) -> Array ErrMsg
  errMsgArr = case _ of
    Just (Left e) -> [ e ]
    _ -> []

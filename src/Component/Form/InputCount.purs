module Component.Form.InputCount
  ( ErrMsg
  , Input
  , Output
  , Query(..)
  , Result
  , proxy
  , slot
  , slot_
  )
  where

import Prelude

import Component.Form.InputBoundedInt as Base
import Component.RenderUtil (classes)
import Data.Count (Count)
import Data.Either (Either)
import Data.Maybe (Maybe)
import Effect.Aff.Class (class MonadAff)
import Halogen as H
import Halogen.HTML as HH
import Type.Proxy (Proxy(..))

type Slot slot row =
  ( "InputCount" :: H.Slot Query Result slot | row )

proxy = Proxy :: Proxy "InputCount"

slot ::
  forall (query :: Type -> Type) (action :: Type) (m :: Type -> Type) (slot :: Type) (row :: Row Type).
  Ord slot =>
  MonadAff m =>
  slot ->
  Input ->
  (Result -> action) ->
  H.ComponentHTML action (Slot slot row) m
slot id input = HH.slot proxy id component input

slot_ ::
  forall (query :: Type -> Type) (action :: Type) (m :: Type -> Type) (slot :: Type) (row :: Row Type).
  Ord slot =>
  MonadAff m =>
  slot ->
  Input ->
  H.ComponentHTML action (Slot slot row) m
slot_ id input = HH.slot_ proxy id component input

type Input = Int

type Output = Count

type ErrMsg = String

type Result = Either ErrMsg Output

data Query a = GetResult (Result -> a)

type State =
  { input :: Int }

data Action
  = Validated (Either String Output)

component :: forall m. MonadAff m => H.Component Query Int Result m
component =
  H.mkComponent
    { initialState: {input: _}
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
  render {input} =
    HH.div
      [ classes [ "control" ] ]
      [ Validated # Base.slot errorMsg unit (Base.defultInput { value = input }) ]

  errorMsg :: Base.ErrCase -> String
  errorMsg = case _ of
    Base.InvalidInt -> "整数を入力してください"
    Base.OutOfRange b t -> (show b) <> "と" <> (show t) <> "の間で入力してください"

  handleAction :: Action -> H.HalogenM _ _ _ _ _ Unit
  handleAction (Validated ret) = H.raise ret

  handleQuery :: forall a. Query a -> H.HalogenM _ _ _ _ _ (Maybe a)
  handleQuery (GetResult reply) = do
    result <- H.request Base.proxy unit Base.Validate
    pure (reply <$> result)

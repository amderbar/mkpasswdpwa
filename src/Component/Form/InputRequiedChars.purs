module Component.Form.InputRequiedChars
  ( Input
  , Output
  , Query(..)
  , mkInput
  , proxy
  , slot
  , slot_
  ) where

import Prelude

import Component.Form.Dropdown as Dropdown
import Component.Form.InputCharTypeConf as InputCharTypeConf
import Component.RenderUtil (classes, errorDisplay)
import Control.Monad.Maybe.Trans (MaybeT(..), runMaybeT)
import Data.Array (length)
import Data.Array.NonEmpty (NonEmptyArray, fromArray, toArray)
import Data.Either (Either)
import Data.FunctorB ((<~>))
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..), maybe)
import Data.Policy (CharGenSrc(..), CharTypeConf, defaultHiragana, defaultSymbols)
import Data.Set as Set
import Data.Traversable (traverse)
import Data.Tuple (uncurry)
import Effect.Aff.Class (class MonadAff)
import Halogen as H
import Halogen.HTML as HH
import Type.Proxy (Proxy(..))

type Slot slot row =
  ("InputRequiedChars" :: H.Slot Query Output slot | row)

proxy = Proxy :: Proxy "InputRequiedChars"

slot
  :: forall (query :: Type -> Type) (action :: Type) (m :: Type -> Type) (slot :: Type) (row :: Row Type)
   . Ord slot
  => MonadAff m
  => slot
  -> Input
  -> (Output -> action)
  -> H.ComponentHTML action (Slot slot row) m
slot id input = HH.slot proxy id component input

slot_
  :: forall (query :: Type -> Type) (action :: Type) (m :: Type -> Type) (slot :: Type) (row :: Row Type)
   . Ord slot
  => MonadAff m
  => slot
  -> Input
  -> H.ComponentHTML action (Slot slot row) m
slot_ id input = HH.slot_ proxy id component input

type Input = Array { | InputCharTypeConf.Input }

mkInput :: Output -> Input
mkInput = map InputCharTypeConf.mkInput >>> toArray

data Query a = GetResult (Output -> a)

type Output = NonEmptyArray CharTypeConf

type Result = Either String Output

data Action
  = AddConf (CharGenSrc Array)
  | UpdateConf Int (InputCharTypeConf.Output)

type State =
  { confs :: Map Int { | InputCharTypeConf.Input }
  , keySeq :: Int
  , errMsg :: Maybe String
  }

component :: forall m. MonadAff m => H.Component Query Input Output m
component =
  H.mkComponent
    { initialState: \inp ->
      { confs: Map.fromFoldableWithIndex inp
      , keySeq: length inp
      , errMsg: Nothing
      }
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
  render { confs, errMsg } =
    HH.div
      [ classes [ "container" ] ] $
      (Map.toUnfoldable confs <#> uncurry \i c -> InputCharTypeConf.slot i c (UpdateConf i))
        <>
          [ AddConf # Dropdown.slot unit
              { isActive: false
              , label: "Add character type"
              , contents:
                  [ Dropdown.Item (mkDropdownItem Digits)
                  , Dropdown.Item (mkDropdownItem UppercaseAlphabets)
                  , Dropdown.Item (mkDropdownItem LowercaseAlphabets)
                  , Dropdown.Item $ mkDropdownItem $ toArray <~> defaultSymbols
                  , Dropdown.Item $ mkDropdownItem $ toArray <~> defaultHiragana
                  , Dropdown.Divider
                  , Dropdown.Item $ mkDropdownItem (AnyChars [])
                  ]
              }
          , errorDisplay $ maybe [] pure errMsg
          ]

  mkDropdownItem :: forall a. CharGenSrc Array -> { onClick :: a -> CharGenSrc Array, label :: String }
  mkDropdownItem src = { onClick: \_ -> src, label: InputCharTypeConf.srcTypeLabel src }

  handleAction :: Action -> H.HalogenM _ _ _ _ _ Unit
  handleAction = case _ of
    AddConf genSrc -> do
      { confs: c, keySeq: k } <- H.get
      let
        keySeq = k + 1
        confs = Map.insert keySeq { count: 0, genSrc } c
      H.modify_ _ { confs = confs, keySeq = keySeq, errMsg = Nothing }
    UpdateConf _ (InputCharTypeConf.Changed _) -> pure unit
    UpdateConf i (InputCharTypeConf.DeleteAtempted) -> do
      confs <- H.gets (_.confs >>> Map.delete i)
      let
        errMsg =
          if Map.size confs > 0 then Nothing
          else Just "文字種を一つ以上指定してください"
      H.modify_ _ { confs = confs, errMsg = errMsg }

  handleQuery :: forall a. Query a -> H.HalogenM _ _ _ _ _ (Maybe a)
  handleQuery (GetResult reply) = runMaybeT $ do
    keys <- MaybeT $ H.gets (_.confs >>> Map.keys >>> Set.toUnfoldable >>> Just)
    confs <- keys # traverse \k -> MaybeT $ H.request InputCharTypeConf.proxy k InputCharTypeConf.GetResult
    MaybeT $ pure $ reply <$> (fromArray confs)

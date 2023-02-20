module Component.Form.InputCharTypeConf
  ( Input
  , Output(..)
  , Query(..)
  , mkInput
  , proxy
  , slot
  , slot_
  , srcTypeLabel
  ) where

import Prelude

import Component.Form.InputCount as InputCount
import Component.Form.InputGenSource as InputGenSrc
import Component.RenderUtil (classes, errorDisplay)
import Data.Array.NonEmpty (NonEmptyArray, toArray)
import Data.Count (Count, fromCount)
import Data.Either (Either(..))
import Data.FunctorB ((<~>))
import Data.Maybe (Maybe(..))
import Data.Policy (CharGenSrc(..), CharTypeConf)
import Data.Validation.Semigroup (V(..), toEither)
import Effect.Aff.Class (class MonadAff)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Record.Builder as R
import Type.Proxy (Proxy(..))

srcTypeLabel :: forall a. CharGenSrc a -> String
srcTypeLabel = case _ of
  Digits -> "Numeral"
  UppercaseAlphabets -> "Uppercase Alphabet"
  LowercaseAlphabets -> "Lowercase Alphabet"
  Symbols _ -> "Symbol"
  Hiraganas _ -> "ひらがな"
  AnyChars _ -> "Custom Charctor Set"

type Slot slot row =
  ("InputCharTypeConf" :: H.Slot Query Output slot | row)

proxy = Proxy :: Proxy "InputCharTypeConf"

slot
  :: forall (action :: Type) (m :: Type -> Type) (slot :: Type) (row :: Row Type)
   . Ord slot
  => MonadAff m
  => slot
  -> { | Input }
  -> (Output -> action)
  -> H.ComponentHTML action (Slot slot row) m
slot id input = HH.slot proxy id component input

slot_
  :: forall (action :: Type) (m :: Type -> Type) (slot :: Type) (row :: Row Type)
   . Ord slot
  => MonadAff m
  => slot
  -> { | Input }
  -> H.ComponentHTML action (Slot slot row) m
slot_ id input = HH.slot_ proxy id component input

type Input =
  ( count :: Int
  , genSrc :: CharGenSrc Array
  )

mkInput :: CharTypeConf -> { | Input }
mkInput { count: c, genSrc: s } =
  { count: fromCount c
  , genSrc: toArray <~> s
  }

data Output
  = Changed CharTypeConf
  | DeleteAtempted

data Query a = GetResult (CharTypeConf -> a)

data Action
  = InputCount InputCount.Result
  | InputGenSrc InputGenSrc.Result
  | DeleteConf

type ErrMsg = String

type Result = Either (Array ErrMsg) CharTypeConf

type State =
  { errMsg :: Array ErrMsg
  | Input
  }

component :: forall m. MonadAff m => H.Component Query { | Input } Output m
component =
  H.mkComponent
    { initialState: R.build (R.merge { errMsg: [] })
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
  render { count, genSrc, errMsg } =
    HH.div
      [ classes [ "field" ] ]
      [ HH.label
          [ classes [ "label", "is-flex", "is-align-items-center", "is-justify-content-space-between" ] ]
          [ HH.text (srcTypeLabel genSrc)
          , HH.button
              [ classes [ "delete", "is-small" ]
              , HE.onClick \_ -> DeleteConf
              ]
              []
          ]
      , HH.div
          [ classes [ "field", "has-addons" ] ]
          [ InputCount # InputCount.slot unit count
          , InputGenSrc # InputGenSrc.slot unit InputGenSrc.defaultInput { genSrc = genSrc }
          ]
      , errorDisplay errMsg
      ]

  handleAction :: Action -> H.HalogenM _ _ _ _ _ Unit
  handleAction = case _ of
    InputCount cntRet -> do
      srcRet <- H.request InputGenSrc.proxy unit InputGenSrc.Validate
      case mkConfResult (Just cntRet) srcRet of
        Left err -> H.modify_ _ { errMsg = err }
        Right conf -> H.modify_ _ { errMsg = [] } *> H.raise (Changed conf)
    InputGenSrc srcRet -> do
      cntRet <- H.request InputCount.proxy unit InputCount.GetResult
      case mkConfResult cntRet (Just srcRet) of
        Left err -> H.modify_ _ { errMsg = err }
        Right conf -> H.modify_ _ { errMsg = [] } *> H.raise (Changed conf)
    DeleteConf -> H.raise DeleteAtempted

  handleQuery :: forall a. Query a -> H.HalogenM _ _ _ _ _ (Maybe a)
  handleQuery (GetResult reply) = do
    cntRet <- H.request InputCount.proxy unit InputCount.GetResult
    srcRet <- H.request InputGenSrc.proxy unit InputGenSrc.Validate
    case mkConfResult cntRet srcRet of
      Left err -> H.modify_ _ { errMsg = err } $> Nothing
      Right conf -> H.modify_ _ { errMsg = [] } $> Just (reply conf)

joinResult :: forall e a. Maybe (Either e a) -> Either (Array e) a
joinResult = case _ of
  Just (Right r) -> Right r
  Just (Left e) -> Left [ e ]
  Nothing -> Left []

mkConfResult :: Maybe (Either ErrMsg Count) -> Maybe (Either ErrMsg (CharGenSrc NonEmptyArray)) -> Either (Array ErrMsg) CharTypeConf
mkConfResult count genSrc = toEither $ ado
  c <- V $ joinResult count
  s <- V $ joinResult genSrc
  in { count: c, genSrc: s }

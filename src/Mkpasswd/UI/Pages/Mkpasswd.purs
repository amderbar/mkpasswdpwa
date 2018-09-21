module Mkpasswd.UI.Pages.Mkpasswd where

import Prelude
import Mkpasswd                   (mkpasswd)
import Mkpasswd.Data.PasswdPolicy (PasswdPolicy, defaultLength, defaultPolicy)
import Mkpasswd.Data.PasswdPolicy.Validation (validate)
import Mkpasswd.Data.Validation   (ErrorCode(..))
import Mkpasswd.Data.Array        (modifyAt)
import Mkpasswd.Data.Tuple        (updateFst, updateSnd, modifyFst, modifySnd)
import Mkpasswd.Halogen.Util      (classes)
import Mkpasswd.UI.Components.MultiChkboxes as MC
import Data.Array                 ((!!), mapWithIndex, mapMaybe, replicate, zip)
import Data.Char                  (fromCharCode)
import Data.Const                 (Const)
import Data.Foldable              (length)
import Data.Generic.Rep           (class Generic)
import Data.Generic.Rep.Show      (genericShow)
import Data.Maybe                 (Maybe(..), fromMaybe, isJust)
import Data.Either                (Either(..), note)
import Data.Int                   (fromString, toNumber)
import Data.String.CodeUnits      (singleton)
import Data.Traversable           (traverse)
import Data.Tuple                 (Tuple(..), fst, snd, uncurry)
import Data.Validation.Semigroup  (V, invalid, toEither)
import Effect.Aff                 (Aff)
import Halogen                 as H
import Halogen.Component.ChildPath as HC
import Halogen.Data.Prism          (type (<\/>), type (\/))
import Halogen.HTML            as HH
import Halogen.HTML.Events     as HE
import Halogen.HTML.Properties as HP

type ChildQuery = MC.Query <\/> Const Void
type Slot  = Unit \/ Void

cpMultichk :: HC.ChildPath MC.Query ChildQuery Unit Slot
cpMultichk = HC.cp1

type PolicyState = Tuple Int (Array (Tuple Boolean Int))

policyState :: PasswdPolicy -> PolicyState
policyState (Tuple n arr) = Tuple n $ zip (replicate (length arr) true) arr

toMaybe :: forall a. Tuple Boolean a -> Maybe a
toMaybe (Tuple flg pol) = if flg then Just pol else Nothing

type AsciiPolicyState =
            { degit     :: Tuple Boolean PolicyState
            , uppercase :: Tuple Boolean PolicyState
            , lowercase :: Tuple Boolean PolicyState
            , symbol    :: Tuple Boolean PolicyState
            }

statePolicy :: AsciiPolicyState -> Array PasswdPolicy
statePolicy p = mapMaybe (toMaybe <<< (map (map (mapMaybe toMaybe))))
    [ p.degit
    , p.uppercase
    , p.lowercase
    , p.symbol
    ]

type State =
    { length :: Int
    , policy :: AsciiPolicyState
    , passwd :: Maybe String
    , errMsg :: Maybe (Array String)
    , custom :: Tuple Boolean Boolean
    }

data Query a
    = Regenerate a
    | UpdateLength String a
    | UpdatePolicy FieldType String a
    | UpdateChecked FieldType Boolean a
    | UpdateCharUse FieldType (Array (Tuple Boolean Int)) a
    | OpenCustom a
    | CloseCustom a

data FieldType
    = DegitsNum
    | UppercaseNum
    | LowercaseNum
    | SymbolNum

derive instance genericFieldType :: Generic FieldType _
instance showFieldType :: Show FieldType where
    show = genericShow

ui :: H.Component HH.HTML Query Unit Void Aff
ui =
  H.parentComponent
    { initialState: const initialState
    , render
    , eval
    , receiver : const Nothing
    }
    where
          initialState :: State
          initialState =
              let d = defaultPolicy
               in
                  { length : defaultLength
                  , policy : { degit     : tuple $ policyState <$> (d !! 0)
                             , uppercase : tuple $ policyState <$> (d !! 1)
                             , lowercase : tuple $ policyState <$> (d !! 2)
                             , symbol    : tuple $ policyState <$> (d !! 3)
                             }
                  , passwd : Nothing
                  , errMsg : Nothing
                  , custom : Tuple false true
                  }
          tuple m = Tuple (isJust m) (fromMaybe (Tuple 0 []) m)

          render :: State -> H.ParentHTML Query ChildQuery Slot Aff
          render state =
                HH.div
                    [ classes [ "flex-auto" , "flex", "flex-column" ] ]
                    [ HH.h1 [ classes [ "center" ] ] [ HH.text "Mkpasswd" ]
                    , errorView $ show <$> state.errMsg
                    , lengthFormRow "ながさ：" $ state.length
                    , policyFormRow DegitsNum    "すうじ：" $ state.policy.degit
                    , policyFormRow UppercaseNum "英大字：" $ state.policy.uppercase
                    , policyFormRow LowercaseNum "英小字：" $ state.policy.lowercase
                    , policyFormRow SymbolNum    "きごう：" $ state.policy.symbol
                    , if fst state.custom
                          then HH.slot' cpMultichk unit MC.ui (snd (snd state.policy.symbol)) $ HE.input (UpdateCharUse SymbolNum)
                          else HH.text ""
                    , toggleSelect $ fst state.custom
                    , resultView state.passwd
                    , HH.button
                        [ classes [ "flex-none", "self-center", "p1" ]
                        , HE.onClick (HE.input_ Regenerate)
                        ]
                        [ HH.text "Generate new Password" ]
                    ]
          lengthFormRow labelTxt currVal =
                  HH.div
                     [ classes [ "flex-none", "clearfix" ] ]
                     [ HH.label
                          [ HP.for "PassedLength"
                          , classes [ "pr1", "col", "col-4", "right-align", "align-baseline", "label" ]
                          ]
                          [ HH.text labelTxt ]
                     , HH.input
                          [ HP.type_ HP.InputNumber
                          , HP.id_ "PassedLength"
                          , classes [ "col", "col-3", "input" ]
                          , HP.min $ toNumber 0
                          , HP.max $ toNumber 100
                          , HP.value $ show currVal
                          , HE.onValueInput $ HE.input UpdateLength
                          ]
                     ]
          policyFormRow feildType labelTxt (Tuple currChk (Tuple currVal _)) =
              let inpIdStr = show feildType
               in
                  HH.div
                     [ classes [ "flex-none", "clearfix" ] ]
                     [ HH.label
                        [ HP.for inpIdStr
                        , classes [ "pr1", "col", "col-4", "right-align", "align-baseline", "label" ]
                        ]
                        [ HH.text labelTxt ]
                     , HH.input
                        [ HP.type_ HP.InputNumber
                        , HP.id_ inpIdStr
                        , classes [ "col", "col-3", "input" ]
                        , HP.min $ toNumber 0
                        , HP.max $ toNumber 100
                        , HP.disabled $ not currChk
                        , HP.value $ show currVal
                        , HE.onValueInput $ HE.input (UpdatePolicy feildType)
                        ]
                     , HH.label
                        [ classes [ "pl1", "col", "col-4", "left-align", "align-baseline", "label" ]
                        ]
                        [ HH.input
                            [ HP.type_ HP.InputCheckbox
                            , HP.checked currChk
                            , HE.onChecked $ HE.input (UpdateChecked feildType)
                            ]
                        , HH.text "含める"
                        ]
                     ]

          toggleSelect flg =
              let q = if flg then CloseCustom else OpenCustom
                  t = if flg then "▲ やめる" else "▼ もっと細かく選ぶ"
               in
                 HH.div
                    [ classes [ "flex-none", "clearfix" ] ]
                    [ HH.span [ classes [ "col", "col-4" ] ] [ HH.text "　" ]
                    , HH.a [ classes [ "col", "col-3" ]
                           , HE.onClick (HE.input_ q)
                           ]
                           [ HH.text t ]
                    ]

          resultView Nothing = HH.text ""
          resultView (Just value) =
              HH.p [ classes [ "h3", "center", "border", "rounded" ] ]
                   [ HH.text value ]
          errorView  Nothing = HH.text ""
          errorView  (Just error) =
              HH.p [ classes [ "h3" , "center" , "border" , "border-red" ] ]
                   [ HH.text error ]

          errorMsg OutOfRange   = "長過ぎます"
          errorMsg ValueMissing = "入力してください"
          errorMsg EmptyCharSet = "指定された文字種が空です"
          errorMsg TooShort     = "長さは文字種ごとの必要最低数の総和よりも大きくしてください"
          errorMsg Unknown      = "なんかエラーになったんでリロードしてください"

          eval :: Query ~> H.ParentDSL State Query ChildQuery Slot Void Aff
          eval (Regenerate next) = do
              s <- H.get
              newPasswd <- H.liftEffect $
                  let vp = toEither $ validate s.length (statePolicy s.policy)
                   in case vp of
                           Right prm -> uncurry mkpasswdE prm
                           Left  err -> pure $ Left err
              case newPasswd of
                   Right p -> H.modify_ (_ { errMsg = Nothing, passwd = Just p })
                   Left  e -> H.modify_ (_ { errMsg = Just (errorMsg <$> e) })
              pure next
              where mkpasswdE l s = note [Unknown] <$> mkpasswd l s

          eval (UpdateLength value next) =
             let newValue = note ("長さには数値を入れてください") $ fromString value
              in do
                 s <- H.get
                 case newValue of
                      Right val -> H.modify_ (_ { errMsg = Nothing, length = val })
                      Left  err -> H.modify_ (_ { errMsg = Just [err] })
                 pure next

          eval (UpdatePolicy feildType value next) =
              let feildName = case feildType of
                                   DegitsNum    -> "すうじ"
                                   UppercaseNum -> "英大字"
                                   LowercaseNum -> "英小字"
                                   SymbolNum    -> "きごう"
                  newValue  = note (feildName <> " should be a Number") $ fromString value
               in do
                  state <- H.get
                  case newValue of
                       Right vli -> modifyPolicy feildType state vli
                       Left  err -> H.modify_ (_ { errMsg = Just [err] })
                  pure next
                  where
                        modifyPolicy f s v = do
                            let p = s.policy
                            let newPolicy =
                                  case f of
                                     DegitsNum     -> p { degit     = updateFst v <$> p.degit }
                                     UppercaseNum  -> p { uppercase = updateFst v <$> p.uppercase }
                                     LowercaseNum  -> p { lowercase = updateFst v <$> p.lowercase }
                                     SymbolNum     -> p { symbol    = updateFst v <$> p.symbol }
                            H.modify_ (_ { errMsg = Nothing, policy = newPolicy })
          eval (UpdateChecked feildType flg next) = do
              state <- H.get
              modifyPolicy feildType state flg
              pure next
              where
                    modifyPolicy f s v = do
                       let p = s.policy
                       let newPolicy =
                               case f of
                                    DegitsNum     -> p { degit     = updateFst v p.degit }
                                    UppercaseNum  -> p { uppercase = updateFst v p.uppercase }
                                    LowercaseNum  -> p { lowercase = updateFst v p.lowercase }
                                    SymbolNum     -> p { symbol    = updateFst v p.symbol }
                       H.modify_ (_ { errMsg = Nothing, policy = newPolicy })

          eval (UpdateCharUse fieldType chrs next) = do
              s <- H.get
              let ns = updateSnd chrs <$> s.policy.symbol
              H.modify_ (_ { policy = s.policy { symbol = ns } })
              pure next

          eval (OpenCustom next) = do
              s <- H.get
              H.modify_ (_ { custom = updateFst true s.custom })
              pure next

          eval (CloseCustom next) = do
              s <- H.get
              let ns = map (map (updateFst true)) <$> s.policy.symbol
              H.modify_ (_ { policy = s.policy { symbol = ns }
                           , custom = updateFst false $ updateSnd true s.custom
                           }
                        )
              pure next

module Mkpasswd.Component.Mkpasswd where

import Prelude
import Mkpasswd                   (mkpasswd)
import Mkpasswd.Data.PasswdPolicy (PasswdPolicy, defaultLength, defaultPolicy)
import Mkpasswd.Halogen.Util      (classes)
import Data.Array                 ((!!), catMaybes)
import Data.Generic.Rep           (class Generic)
import Data.Generic.Rep.Show      (genericShow)
import Data.Maybe                 (Maybe(..), fromMaybe, isJust)
import Data.Either                (Either(..), note)
import Data.Int                   (fromString)
import Data.String.CodePoints     (length)
import Data.Tuple                 (Tuple(..))
import Effect.Aff                 (Aff)
import Halogen                 as H
import Halogen.HTML            as HH
import Halogen.HTML.Events     as HE
import Halogen.HTML.Properties as HP

type State =
    { length :: Int
    , policy :: { degit     :: Tuple Boolean PasswdPolicy
                , uppercase :: Tuple Boolean PasswdPolicy
                , lowercase :: Tuple Boolean PasswdPolicy
                , symbol    :: Tuple Boolean PasswdPolicy
                }
    , passwd :: String
    , errMsg :: String
    }

data Query a
    = Regenerate a
    | UpdateLength String a
    | UpdatePolicy FieldType String a
    | UpdateChecked FieldType Boolean a

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
  H.component
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
                  , policy : { degit     : tuple (d !! 0)
                             , uppercase : tuple (d !! 1)
                             , lowercase : tuple (d !! 2)
                             , symbol    : tuple (d !! 3)
                             }
                  , passwd : ""
                  , errMsg : ""
                  }
          tuple m = Tuple (isJust m) (fromMaybe (Tuple 0 []) m)

          render :: State -> H.ComponentHTML Query
          render state =
                HH.div
                    [ classes [ "flex-auto" , "flex", "flex-column" ] ]
                    [ HH.h1 [ classes [ "center" ] ] [ HH.text "Mkpasswd" ]
                    , errorView state.errMsg
                    , lengthFormRow "ながさ：" $ state.length
                    , policyFormRow DegitsNum    "すうじ：" $ state.policy.degit
                    , policyFormRow UppercaseNum "英大字：" $ state.policy.uppercase
                    , policyFormRow LowercaseNum "英小字：" $ state.policy.lowercase
                    , policyFormRow SymbolNum    "きごう：" $ state.policy.symbol
                    , resultView state.passwd
                    , HH.button
                        [ classes [ "self-center", "p1" ]
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
          resultView value =
              if length value > 0
                  then HH.p [ classes [ "h3", "center", "border", "rounded" ] ]
                            [ HH.text value ]
                  else HH.text value
          errorView  error =
              if length error > 0
                  then HH.p [ classes [ "h3" , "center" , "border" , "border-red" ] ]
                            [ HH.text error ]
                  else HH.text error

          eval :: Query ~> H.ComponentDSL State Query Void Aff
          eval (Regenerate next) = do
              s <- H.get
              newPasswd <- H.liftEffect $
                mkpasswd s.length $
                  catMaybes [ toMaybe s.policy.degit
                            , toMaybe s.policy.uppercase
                            , toMaybe s.policy.lowercase
                            , toMaybe s.policy.symbol
                            ]
              H.modify_ (_ { errMsg = "", passwd = newPasswd })
              pure next
              where
                    toMaybe (Tuple flg pol) = if flg then Just pol else Nothing
          eval (UpdateLength value next) =
             let newValue = note ("PasswdLength should be a Number") $ fromString value
              in do
                 s <- H.get
                 case newValue of
                      Right val -> H.modify_ (_ { errMsg = "", length = val })
                      Left  err -> H.modify_ (_ { errMsg = err })
                 pure next

          eval (UpdatePolicy feildType value next) =
              let newValue = note (show feildType <> " should be a Number") $ fromString value
               in do
                  state <- H.get
                  case newValue of
                       Right vli -> modifyPolicy feildType state vli
                       Left  err -> H.modify_ (_ { errMsg = err })
                  pure next
                  where
                        updateTuple (Tuple b (Tuple _ a)) n = Tuple b (Tuple n a)
                        modifyPolicy f s v = do
                            let p = s.policy
                            let newPolicy =
                                  case f of
                                     DegitsNum     -> p { degit     = updateTuple p.degit v }
                                     UppercaseNum  -> p { uppercase = updateTuple p.uppercase v }
                                     LowercaseNum  -> p { lowercase = updateTuple p.lowercase v }
                                     SymbolNum     -> p { symbol    = updateTuple p.symbol v }
                            H.modify_ (_ { errMsg = "", policy = newPolicy })
          eval (UpdateChecked feildType flg next) = do
              state <- H.get
              modifyPolicy feildType state flg
              pure next
              where
                    updateTuple (Tuple _ i) f = Tuple f i
                    modifyPolicy f s v = do
                       let p = s.policy
                       let newPolicy =
                               case f of
                                    DegitsNum     -> p { degit     = updateTuple p.degit v }
                                    UppercaseNum  -> p { uppercase = updateTuple p.uppercase v }
                                    LowercaseNum  -> p { lowercase = updateTuple p.lowercase v }
                                    SymbolNum     -> p { symbol    = updateTuple p.symbol v }
                       H.modify_ (_ { errMsg = "", policy = newPolicy })

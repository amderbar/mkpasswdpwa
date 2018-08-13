module Mkpasswd.Component.Mkpasswd where

import Prelude
import Mkpasswd                  (mkpasswd, PasswdPolicy, defaultPolicy)
import Mkpasswd.Halogen.Util     (classes, style)
import Data.Generic.Rep          (class Generic)
import Data.Generic.Rep.Show     (genericShow)
import Data.Maybe                (Maybe(..))
import Data.Either               (Either(..), note)
import Data.Int                  (fromString)
import Data.String.CodePoints    (length)
import Effect.Aff                (Aff)
import Halogen                 as H
import Halogen.HTML            as HH
import Halogen.HTML.Events     as HE
import Halogen.HTML.Properties as HP

type State =
    { policy :: PasswdPolicy
    , passwd :: String
    , errMsg :: String
    }

data Query a
    = Regenerate a
    | UpdatePolicy FieldType String a

data FieldType
    = PasswdLength
    | DegitsNum
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
              { policy : defaultPolicy
              , passwd : ""
              , errMsg : ""
              }

          render :: State -> H.ComponentHTML Query
          render state =
                HH.div
                    [ classes [ "flex-auto" , "flex", "flex-column" ] ]
                    [ HH.h1 [ classes [ "center" ] ] [ HH.text "Mkpasswd" ]
                    , errorView state.errMsg
                    , policyFormRow PasswdLength "ながさ：" $ show state.policy.length
                    , policyFormRow DegitsNum    "すうじ：" $ show state.policy.degit
                    , policyFormRow UppercaseNum "英大字：" $ show state.policy.uppercase
                    , policyFormRow LowercaseNum "英小字：" $ show state.policy.lowercase
                    , policyFormRow SymbolNum    "きごう：" $ show state.policy.symbol
                    , resultView state.passwd
                    , HH.button
                        [ classes [ "self-center", "p1" ]
                        , HE.onClick (HE.input_ Regenerate)
                        ]
                        [ HH.text "Generate new Password" ]
                    ]
          policyFormRow feildType labelTxt currentValue =
              let inpIdStr = show feildType
               in
                  HH.div
                     [ classes [ "flex-none", "clearfix" ] ]
                     [ HH.label
                        [ HP.for inpIdStr
                        , classes [ "col", "col-3", "right-align", "align-baseline", "label" ]
                        ]
                        [ HH.text labelTxt ]
                     , HH.input
                        [ HP.type_ HP.InputNumber
                        , HP.id_ inpIdStr
                        , classes [ "col", "col-8", "input" ]
                        , HP.value currentValue
                        , HE.onValueInput $ HE.input (UpdatePolicy feildType)
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
              state <- H.get
              newPasswd <- H.liftEffect $ mkpasswd state.policy
              case newPasswd of
                   Right pass -> H.modify_ (_ { errMsg = "", passwd = pass })
                   Left  err  -> H.modify_ (_ { errMsg = err  })
              pure next
          eval (UpdatePolicy feildType value next) =
              let newValue = note (show feildType <> " should be a Number") $ fromString value
               in do
                  state <- H.get
                  case newValue of
                       Right vli -> modifyPolicy feildType state vli
                       Left  err -> H.modify_ (_ { errMsg = err })
                  pure next
          modifyPolicy f s v = do
              let newPolicy =
                    case f of
                       PasswdLength  -> s.policy { length    = v }
                       DegitsNum     -> s.policy { degit     = v }
                       UppercaseNum  -> s.policy { uppercase = v }
                       LowercaseNum  -> s.policy { lowercase = v }
                       SymbolNum     -> s.policy { symbol    = v }
              H.modify_ (_ { errMsg = "", policy = newPolicy })

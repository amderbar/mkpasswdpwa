module Mkpasswd.UI.Components.PolicyFormRow where

import Prelude
import Data.Const                                  (Const)
import Data.Int                                    (toNumber)
import Data.Maybe                                  (Maybe(..))
import DOM.HTML.Indexed.StepValue                  (StepValue(..))
import Effect.Class                                (class MonadEffect)
import Halogen                                   as H
import Halogen.Component.ChildPath               as HC
import Halogen.Data.Prism                          (type (<\/>), type (\/))
import Halogen.HTML                              as HH
import Halogen.HTML.Events                       as HE
import Halogen.HTML.Properties                   as HP

import Mkpasswd.Data.FieldType.Mkpasswd             (FieldType, labelTxt)
import Mkpasswd.Halogen.Util                        (classes)
import Mkpasswd.UI.Components.LabeledInputNumber as LblInp

type Input =
    { fieldType      :: FieldType
    , isUsed         :: Boolean
    , requiredMinNum :: String
    }
type Message = Input
type State = Input

data Query a
    = OnInputRequiredMinNum LblInp.Message a
    | OnChkUse Boolean a

ui :: forall m. MonadEffect m => H.Component HH.HTML Query Input Message m
ui =
  H.component
    { initialState
    , render
    , eval
    , receiver : const Nothing
    }
    where
          initialState :: Input -> State
          initialState i = i

          render :: State -> H.ComponentHTML Query
          render state =
             HH.div
                [ classes [ "field" ] ]
                [ HH.label
                    [ classes [ "label" ] ]
                    [ HH.text (labelTxt state.fieldType) ]
                , HH.div
                    [ classes [ "field", "has-addons" ] ]
                    [ HH.span
                        [ classes [ "control" ] ]
                        [ HH.label
                            [ classes [ "button", "checkbox" ]
                            ]
                            [ HH.input
                                [ HP.type_ HP.InputCheckbox
                                , HP.checked state.isUsed
                                , classes [ "mr1" ]
                                , HE.onChecked $ HE.input OnChkUse
                                ]
                            , HH.text "use it"
                            ]
                        ]
                    , HH.span
                        [ classes [ "control", "is-expanded" ] ]
                        [ HH.input
                            [ HP.type_ HP.InputNumber
                            , HP.id_ (show state.fieldType)
                            , classes [ "input" ]
                            , HP.value state.requiredMinNum
                            , HE.onValueInput $ HE.input OnInputRequiredMinNum
                            , HP.step Any
                            , HP.min (toNumber 0)
                            , HP.max (toNumber 100)
                            , HP.disabled (not state.isUsed)
                            ]
                        ]
                    ]
                ]


          eval :: Query ~> H.ComponentDSL State Query Message m
          eval (OnInputRequiredMinNum mi next) = do
             s <- H.get
             let ns = s { requiredMinNum = mi }
             H.raise ns
             H.put ns
             pure next

          eval (OnChkUse f next) = do
             s <- H.get
             let ns = s { isUsed = f }
             H.raise ns
             H.put ns
             pure next

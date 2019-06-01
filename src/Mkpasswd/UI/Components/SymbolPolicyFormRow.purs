module Mkpasswd.UI.Components.SymbolPolicyFormRow where

import Prelude
import Data.Const                             (Const)
import Data.Int                               (toNumber)
import Data.Maybe                             (Maybe(..))
import DOM.HTML.Indexed.StepValue             (StepValue(..))
import Effect.Class                           (class MonadEffect)
import Halogen                              as H
import Halogen.Component.ChildPath          as HC
import Halogen.Data.Prism                     (type (<\/>), type (\/))
import Halogen.HTML                         as HH
import Halogen.HTML.Events                  as HE
import Halogen.HTML.Properties        as HP

import Mkpasswd.Data.FieldType.Mkpasswd       (FieldType, labelTxt)
import Mkpasswd.Data.Switch                   (Switch)
import Mkpasswd.Halogen.Util                  (classes)
import Mkpasswd.UI.Components.PolicyFormRow as PolRow
import Mkpasswd.UI.Components.MultiChkboxes as MulChk

type ChildQuery = PolRow.Query <\/> MulChk.Query <\/> Const Void
type Slot  = Unit \/ Unit \/ Void

cpPolRow :: HC.ChildPath PolRow.Query ChildQuery Unit Slot
cpPolRow = HC.cp1

cpMulChk :: HC.ChildPath MulChk.Query ChildQuery Unit Slot
cpMulChk = HC.cp2

type Input =
    { fieldType      :: FieldType
    , isUsed         :: Boolean
    , requiredMinNum :: String
    , isOpenMulChk   :: Boolean
    , allChk         :: Boolean
    , chars          :: Array (Switch Int)
    }
type Message =
    { fieldType      :: FieldType
    , isUsed         :: Boolean
    , requiredMinNum :: String
    , chars          :: Array (Switch Int)
    }
type State = Input

data Query a
    = OnInputMinNum PolRow.Message a
    | OnChangeChrChk MulChk.Message a

ui :: forall m. MonadEffect m => H.Component HH.HTML Query Input Message m
ui =
  H.parentComponent
    { initialState
    , render
    , eval
    , receiver : const Nothing
    }
    where
          initialState :: Input -> State
          initialState i = i

          render :: State -> H.ParentHTML Query ChildQuery Slot m
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
                                , HE.onChecked $ HE.input (OnInputMinNum <<< {fieldType: state.fieldType, requiredMinNum: state.requiredMinNum, isUsed: _})
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
                            , HE.onValueInput $ HE.input (OnInputMinNum <<< {fieldType: state.fieldType, requiredMinNum: _, isUsed: state.isUsed})
                            , HP.step Any
                            , HP.min (toNumber 0)
                            , HP.max (toNumber 100)
                            , HP.disabled (not state.isUsed)
                            ]
                        ]
                    ]
                , (\i -> HH.slot' cpMulChk unit MulChk.ui i $ HE.input OnChangeChrChk)
                    { allChk: state.allChk
                    , chars : state.chars
                    , isOpenMulChk: state.isOpenMulChk
                    , disabled: not state.isUsed
                    }
                ]

          eval :: Query ~> H.ParentDSL State Query ChildQuery Slot Message m
          eval (OnInputMinNum m next) = do
             ns <- H.modify (_ { isUsed = m.isUsed, requiredMinNum = m.requiredMinNum })
             H.raise $ message ns
             pure next

          eval (OnChangeChrChk m next) = do
             ns <- H.modify (_ { chars = m })
             H.raise $ message ns
             pure next

          message :: State -> Message
          message s =
             { fieldType      : s.fieldType
             , isUsed         : s.isUsed
             , requiredMinNum : s.requiredMinNum
             , chars          : s.chars
             }

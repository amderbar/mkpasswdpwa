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

type ChildQuery = LblInp.Query <\/> Const Void
type ChildSlot  = Unit \/ Void

cpLblInp :: HC.ChildPath LblInp.Query ChildQuery Unit ChildSlot
cpLblInp = HC.cp1

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
  H.parentComponent
    { initialState
    , render
    , eval
    , receiver : const Nothing
    }
    where
          initialState :: Input -> State
          initialState i = i

          render :: forall m. MonadEffect m => State -> H.ParentHTML Query ChildQuery ChildSlot m
          render state =
             HH.div
                [ classes [ "clearfix" ] ]
                [ (\i -> HH.slot' cpLblInp unit LblInp.ui i $ HE.input OnInputRequiredMinNum)
                   { labelTxt: labelTxt state.fieldType
                   , id: show state.fieldType
                   , min: Just $ toNumber 0
                   , max: Just $ toNumber 100
                   , step: Any
                   , disabled: Just $ not state.isUsed
                   , value: state.requiredMinNum
                   }
                , HH.label
                   [ classes [ "label" ]
                   ]
                   [ HH.input
                       [ HP.type_ HP.InputCheckbox
                       , HP.checked state.isUsed
                       , HE.onChecked $ HE.input OnChkUse
                       ]
                   , HH.text "含める"
                   ]
                ]

          eval :: forall m. MonadEffect m => Query ~> H.ParentDSL State Query ChildQuery ChildSlot Message m
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

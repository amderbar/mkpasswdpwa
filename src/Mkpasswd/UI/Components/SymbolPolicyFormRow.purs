module Mkpasswd.UI.Components.SymbolPolicyFormRow where

import Prelude
import Data.Const                             (Const)
import Data.Maybe                             (Maybe(..))
import Effect.Class                           (class MonadEffect)
import Halogen                              as H
import Halogen.Component.ChildPath          as HC
import Halogen.Data.Prism                     (type (<\/>), type (\/))
import Halogen.HTML                         as HH
import Halogen.HTML.Events                  as HE

import Mkpasswd.Data.FieldType.Mkpasswd       (FieldType)
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
    | ToggleMulChk Boolean a

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

          render :: forall m. MonadEffect m => State -> H.ParentHTML Query ChildQuery Slot m
          render state =
             HH.div
                [ classes [ "flex-none" ] ]
                [ (\i -> HH.slot' cpPolRow unit PolRow.ui i $ HE.input OnInputMinNum)
                    { fieldType: state.fieldType
                    , isUsed: state.isUsed
                    , requiredMinNum: state.requiredMinNum
                    }
                , HH.a
                    [ classes [ "block" ]
                    , HE.onClick $ HE.input_ $ ToggleMulChk (not state.isOpenMulChk)
                    ]
                    [ HH.text if state.isOpenMulChk then "▲ やめる" else "▼ もっと細かく選ぶ" ]
                , if state.isOpenMulChk
                      then (\i -> HH.slot' cpMulChk unit MulChk.ui i $ HE.input OnChangeChrChk)
                             { allChk: state.allChk
                             , chars : state.chars
                             }
                      else HH.text ""
                ]

          eval :: forall m. MonadEffect m => Query ~> H.ParentDSL State Query ChildQuery Slot Message m
          eval (OnInputMinNum m next) = do
             ns <- H.modify (_ { isUsed = m.isUsed, requiredMinNum = m.requiredMinNum })
             H.raise $ message ns
             pure next

          eval (OnChangeChrChk m next) = do
             ns <- H.modify (_ { chars = m })
             H.raise $ message ns
             pure next

          eval (ToggleMulChk f next) = do
             H.modify_ (_ { isOpenMulChk = f })
             pure next

          message :: State -> Message
          message s =
             { fieldType      : s.fieldType
             , isUsed         : s.isUsed
             , requiredMinNum : s.requiredMinNum
             , chars          : s.chars
             }

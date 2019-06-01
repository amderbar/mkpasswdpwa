module Mkpasswd.UI.Components.LabeledInputNumber where

import Prelude
import Data.Array                        (catMaybes)
import Data.Maybe                        (Maybe(..))
import DOM.HTML.Indexed.StepValue        (StepValue)
import Halogen                        as H
import Halogen.HTML                   as HH
import Halogen.HTML.Events            as HE
import Halogen.HTML.Properties        as HP

import Mkpasswd.Halogen.Util             (classes)

type Input =
    { labelTxt :: String
    , id       :: String
    , disabled :: Maybe Boolean
    , min      :: Maybe Number
    , max      :: Maybe Number
    , step     :: StepValue
    , value    :: String
    }

type Message = String

type State = Input

data Query a
    = OnReciveInput Input a
    | OnValueInput String a

ui :: forall m. H.Component HH.HTML Query Input Message m
ui =
  H.component
    { initialState
    , render
    , eval
    , receiver : HE.input OnReciveInput
    }
    where
          initialState :: Input -> State
          initialState i = i

          render :: State -> H.ComponentHTML Query
          render state =
             HH.div
                [ classes [ "field" ] ]
                [ HH.label
                    [ HP.for state.id
                    , classes [ "pr1", "label" ]
                    ]
                    [ HH.text state.labelTxt ]
                , HH.span
                    [ classes [ "control" ] ]
                    [ HH.input $
                        catMaybes
                        [ Just $ HP.type_ HP.InputNumber
                        , Just $ HP.id_ state.id
                        , Just $ classes [ "input" ]
                        , Just $ HP.value state.value
                        , Just $ HE.onValueInput $ HE.input OnValueInput
                        , Just $ HP.step state.step
                        , HP.max      <$> state.max
                        , HP.min      <$> state.min
                        , HP.disabled <$> state.disabled
                        ]
                    ]
                ]

          eval :: forall m. Query ~> H.ComponentDSL State Query Message m
          eval (OnReciveInput i next) = do
             s <- H.get
             --H.put $ i { value = fromMaybe s.value (show <$> i.value) }
             H.put $ i { value = s.value }
             pure next

          eval (OnValueInput n next) = do
             s <- H.get
             H.modify_ (_ { value = n })
             H.raise $  n
             pure next

module Mkpasswd.UI.Components.MultiChkboxes where

import Prelude
import Data.Array                 (mapWithIndex)
import Data.Char                  (fromCharCode)
import Data.Maybe                 (Maybe(..), fromMaybe)
import Data.String.CodeUnits      (singleton)
import Halogen                 as H
import Halogen.HTML            as HH
import Halogen.HTML.Events     as HE
import Halogen.HTML.Properties as HP

import Mkpasswd.Data.Array        (modifyAt)
import Mkpasswd.Data.Switch       (Switch)
import Mkpasswd.Data.Switch    as Switch
import Mkpasswd.Data.Tuple        (updateFst)
import Mkpasswd.Halogen.Util      (classes)

type Input =
    { allChk :: Boolean
    , chars  :: Array (Switch Int)
    }
type Message = Array (Switch Int)
type State = Input

data Query a
    = OnCheck Int a
    | OnChkAll Boolean a

ui :: forall m. H.Component HH.HTML Query Input Message m
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
                 [ classes [ "flex-none", "clearfix" ] ]
                 [ HH.div
                     [ classes [ "sm-col", "sm-col-12", "md-col", "md-col-9", "lg-col", "lg-col-6" ] ]
                     $ join
                         [ flip mapWithIndex state.chars (\i c ->
                             HH.label
                                [ classes [ "col", "col-4", "center", "align-baseline", "label" ] ]
                                [ HH.input
                                    [ HP.type_ HP.InputCheckbox
                                    , HP.checked $ Switch.isOn c
                                    , HE.onChange $ HE.input_ (OnCheck i)
                                    ]
                                , HH.text $ singleton $ fromMaybe '?' $ fromCharCode (Switch.label c)
                                ]
                           )
                         , [ HH.label
                                [ classes [ "col", "col-4", "center", "align-baseline", "label" ] ]
                                [ HH.input
                                    [ HP.type_ HP.InputCheckbox
                                    , HP.checked state.allChk
                                    , HE.onChecked $ HE.input OnChkAll
                                    ]
                                , HH.text $ if state.allChk then "ぜんぶ外す" else "ぜんぶ付ける"
                                ]
                           ]
                         ]
                 ]

          eval :: forall m. Query ~> H.ComponentDSL State Query Message m
          eval (OnCheck idx next) = do
              s <- H.get
              let ns = modifyAt idx Switch.toggle s.chars
              H.raise ns
              H.modify_ (_ { chars = ns })
              pure next

          eval (OnChkAll flg next) = do
              s <- H.get
              let ns = turn <$> s.chars
              H.raise ns
              H.modify_ (_ { allChk = flg, chars = ns })
              pure next
              where
                    turn :: forall a. Switch a -> Switch a
                    turn = if flg then Switch.on else Switch.off

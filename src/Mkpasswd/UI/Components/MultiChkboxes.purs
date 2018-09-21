module Mkpasswd.UI.Components.MultiChkboxes where

import Prelude
import Mkpasswd.Data.Array        (modifyAt)
import Mkpasswd.Data.Tuple        (updateFst)
import Mkpasswd.Halogen.Util      (classes)
import Data.Array                 (mapWithIndex)
import Data.Char                  (fromCharCode)
import Data.Maybe                 (Maybe(..), fromMaybe)
import Data.String.CodeUnits      (singleton)
import Data.Tuple                 (Tuple(..), fst)
import Data.Foldable              (any)
import Effect.Aff                 (Aff)
import Halogen                 as H
import Halogen.HTML            as HH
import Halogen.HTML.Events     as HE
import Halogen.HTML.Properties as HP

type Input = Array (Tuple Boolean Int)

type Message = Input

type State =
    { chars     :: Input
    , allChkFlg :: Boolean
    }

data Query a
    = UpdateChk Int Boolean a
    | UpdateChkAll Boolean a

ui :: H.Component HH.HTML Query Input Message Aff
ui =
  H.component
    { initialState
    , render
    , eval
    , receiver : const Nothing
    }
    where
          initialState :: Input -> State
          initialState i = { chars     : i
                           , allChkFlg : any fst i
                           }

          render :: State -> H.ComponentHTML Query
          render state =
              HH.div
                 [ classes [ "flex-none", "clearfix" ] ]
                 [ HH.div
                     [ classes [ "sm-col", "sm-col-12", "md-col", "md-col-9", "lg-col", "lg-col-6" ] ]
                     $ join [ (mapWithIndex charCheck state.chars)
                            , [ allCheck state.allChkFlg ]
                            ]
                 ]

          charCheck idx (Tuple chk chr) =
              HH.label
                 [ classes [ "col", "col-4", "center", "align-baseline", "label" ]
                 ]
                 [ HH.input
                     [ HP.type_ HP.InputCheckbox
                     , HP.checked chk
                     , HE.onChecked $ HE.input (UpdateChk idx)
                     ]
                 , HH.text $ singleton $ fromMaybe '?' $ fromCharCode chr
                 ]

          allCheck currChk =
              HH.label
                 [ classes [ "col", "col-4", "center", "align-baseline", "label" ]
                 ]
                 [ HH.input
                     [ HP.type_ HP.InputCheckbox
                     , HP.checked currChk
                     , HE.onChecked $ HE.input UpdateChkAll
                     ]
                 , HH.text $ if currChk then "ぜんぶ外す" else "ぜんぶ付ける"
                 ]

          eval :: Query ~> H.ComponentDSL State Query Message Aff
          eval (UpdateChk idx flg next) = do
              s <- H.get
              let ns = modifyAt idx (updateFst flg) s.chars
              H.raise ns
              H.modify_ (_ { chars = ns })
              pure next

          eval (UpdateChkAll flg next) = do
              s <- H.get
              let ns = (updateFst flg) <$> s.chars
              H.raise ns
              H.modify_ (_ { chars     = ns
                           , allChkFlg = flg
                           }
                        )
              pure next

module Mkpasswd.UI.Components.MultiChkboxes where

import Prelude
import Data.Array                 (mapWithIndex, (:))
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
import Mkpasswd.Halogen.Util      (classes)

type Input =
    { allChk :: Boolean
    , chars  :: Array (Switch Int)
    , isOpenMulChk   :: Boolean
    , disabled :: Boolean
    }
type Message = Array (Switch Int)
type State = Input

data Query a
    = OnCheck Int a
    | OnChkAll Boolean a
    | ToggleMulChk Boolean a
    | OnModInput Input a

ui :: forall m. H.Component HH.HTML Query Input Message m
ui =
  H.component
    { initialState
    , render
    , eval
    , receiver : HE.input OnModInput
    }
    where
          initialState :: Input -> State
          initialState i = i

          render :: State -> H.ComponentHTML Query
          render state =
              HH.div
                 [ classes [ "card" ] ]
                 [ HH.header
                    [ classes [ "card-header" ]
                    , HE.onClick $ HE.input_ $ ToggleMulChk (not state.isOpenMulChk)
                    ]
                    [ HH.p
                        [ classes [ "card-header-title" ] ]
                        [ HH.text "Individual symbol" ]
                    , HH.a
                        [ classes [ "card-header-icon" ]
                        , HP.attr (HH.AttrName "aria-label") "more options"
                        ]
                        [ HH.span
                            [ classes ["icon"] ]
                            [ HH.text "▼" ]
                        ]
                    ]
                 , if state.isOpenMulChk
                    then HH.fieldset
                        [ classes [ "card-content" ]
                        , HP.disabled state.disabled
                        ]
                        $ HH.div
                            [ classes ["field"] ]
                            [ HH.span
                                [ classes [ "controll" ] ]
                                [ HH.label
                                    [ classes [ "checkbox" ] ]
                                    [ HH.input
                                        [ HP.type_ HP.InputCheckbox
                                        , HP.checked state.allChk
                                        , classes [ "mr1" ]
                                        , HP.disabled state.disabled
                                        , HE.onChecked $ HE.input OnChkAll
                                        ]
                                    , HH.text "use all symbol"
                                    ]
                                ]
                            ] : flip mapWithIndex state.chars (\i c ->
                                HH.div
                                    [ classes ["field"] ]
                                    [ HH.span
                                        [ classes [ "controll" ] ]
                                        [ HH.label
                                            [ classes [ "checkbox" ] ]
                                            [ HH.input
                                                [ HP.type_ HP.InputCheckbox
                                                , HP.checked $ Switch.isOn c
                                                , classes [ "mr1" ]
                                                    , HP.disabled state.disabled
                                                , HE.onChange $ HE.input_ (OnCheck i)
                                                ]
                                            , HH.strong_
                                                [ HH.text $ singleton $ fromMaybe '?' $ fromCharCode (Switch.label c) ]
                                            ]
                                        ]
                                    ]
                            )
                    else HH.text ""
                 ]

          eval :: forall m. Query ~> H.ComponentDSL State Query Message m
          eval (ToggleMulChk f next) = do
             H.modify_ (_ { isOpenMulChk = f })
             pure next

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

          eval (OnModInput inp next) = do
              H.modify_ (_ { disabled = inp.disabled })
              pure next

module Component.RenderUtil where

import Prelude
import DOM.HTML.Indexed.StepValue (StepValue(..))
import Data.Int (toNumber)
import Data.Maybe (Maybe, fromMaybe, isJust)
import Effect.Routing (RouteHash(..), hashStr)
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Web.UIEvent.MouseEvent (MouseEvent)

type ErrText = String

classes :: forall a b. Array String -> HP.IProp (class :: String | a) b
classes = HP.classes <<< map HH.ClassName

emptyNode :: forall slot action. HH.HTML slot action
emptyNode = HH.text ""

inputFormContainer
  :: forall id slot action
   . Show id
  => id
  -> String
  -> Array ErrText
  -> HH.HTML slot action
  -> HH.HTML slot action
inputFormContainer formId labelText errArr inpForm =
  HH.div
    [ classes [ "field" ] ]
    [ labelBlock formId labelText
    , inpForm
    , errorDisplay errArr
    ]

labelBlock :: forall id slot action. Show id => id -> String -> HH.HTML slot action
labelBlock fieldType labelText =
  HH.label
    [ HP.for $ show fieldType
    , classes [ "label" ]
    ]
    [ HH.text labelText ]

inputTextForm
  :: forall id slot action
   . Show id
  => id
  -> Boolean
  -> Boolean
  -> String
  -> (String -> action)
  -> HH.HTML slot action
inputTextForm fieldId hasErr enabled inp onInput =
  HH.div
    [ classes [ "control", "is-expanded" ] ]
    [ HH.input
        [ HP.type_ HP.InputText
        , HP.id $ show fieldId
        , classes
            [ "input"
            , if hasErr then "is-danger" else ""
            ]
        , HP.value inp
        , HE.onValueInput onInput
        , HP.disabled (not enabled)
        ]
    ]

inputNumberForm
  :: forall id slot action
   . Show id
  => id
  -> Boolean
  -> Boolean
  -> String
  -> (String -> action)
  -> HH.HTML slot action
inputNumberForm fieldId hasErr enabled inp onInput =
  HH.div
    [ classes [ "control", "is-expanded" ] ]
    [ HH.input
        [ HP.type_ HP.InputNumber
        , HP.id $ show fieldId
        , classes
            [ "input"
            , if hasErr then "is-danger" else ""
            ]
        , HP.value inp
        , HE.onValueInput onInput
        , HP.step Any
        , HP.max (toNumber 100)
        , HP.min (toNumber 0)
        , HP.disabled (not enabled)
        ]
    ]

inputAddon :: forall slot action. Boolean -> (Boolean -> action) -> HH.HTML slot action
inputAddon checked onChecked =
  HH.span
    [ classes [ "control" ] ]
    [ HH.label
        [ classes [ "button", "checkbox" ] ]
        [ HH.input
            [ HP.type_ HP.InputCheckbox
            , HP.checked checked
            , classes [ "mr1" ]
            , HE.onChecked onChecked
            ]
        ]
    ]

textAreaForm
  :: forall id slot action
   . Show id
  => id
  -> Boolean
  -> String
  -> (String -> action)
  -> HH.HTML slot action
textAreaForm fieldId hasErr inp onInput =
  HH.div
    [ classes [ "control" ] ]
    [ HH.textarea
        [ HP.id $ show fieldId
        , classes
            [ "input"
            , if hasErr then "is-danger" else ""
            ]
        , HP.value inp
        , HE.onValueInput onInput
        ]
    ]

errorDisplay :: forall slot action. Array ErrText -> HH.HTML slot action
errorDisplay = case _ of
  [] -> emptyNode
  errArr ->
    HH.ul
      [ classes [ "help", "is-danger" ] ]
      (errArr <#> \t -> HH.li_ [ HH.text t ])

footerBtnArea :: forall slot action. (MouseEvent -> action) -> HH.HTML slot action
footerBtnArea onclick =
  HH.section
    [ classes [ "level", "sticky-bottom", "box" ] ]
    [ HH.div
        [ classes [ "level-item" ] ]
        [ HH.div
            [ classes [ "field", "is-grouped" ] ]
            [ HH.span
                [ classes [ "control" ] ]
                [ HH.button
                    [ classes [ "button", "is-dark" ]
                    , HE.onClick onclick
                    ]
                    [ HH.text "Generate" ]
                ]
            ]
        ]
    ]

resultModal
  :: forall slot action
   . { onclickClose :: (MouseEvent -> action)
     , onclickSave :: (MouseEvent -> action)
     , onclickRegenerate :: (MouseEvent -> action)
     }
  -> Maybe String
  -> HH.HTML slot action
resultModal { onclickClose, onclickSave, onclickRegenerate } result =
  HH.div
    [ classes [ "modal", if isJust result then "is-active" else "" ] ]
    [ HH.div
        [ classes [ "modal-background" ]
        , HE.onClick onclickClose
        ]
        []
    , HH.div
        [ classes [ "modal-card" ] ]
        [ HH.header
            [ classes [ "modal-card-head" ] ]
            [ HH.h1
                [ classes [ "modal-card-title" ] ]
                [ HH.text "Result" ]
            , HH.button
                [ classes [ "delete" ]
                , HP.attr (HH.AttrName "aria-label") "close"
                , HE.onClick onclickClose
                ]
                []
            ]
        , HH.section
            [ classes [ "modal-card-body" ] ]
            [ HH.div
                [ classes [ "level" ] ]
                [ HH.div
                    [ classes [ "level-item", "has-text-centered", "is-size-3", "text-wrap" ] ]
                    [ HH.br_
                    , HH.text $ fromMaybe "" result
                    , HH.br_
                    ]
                ]
            ]
        , HH.footer
            [ classes [ "modal-card-foot" ] ]
            [ HH.a
                [ classes [ "button", "is-dark" ]
                , HE.onClick onclickSave
                , HP.href $ hashStr New
                ]
                [ HH.text "Save" ]
            , HH.button
                [ classes [ "button" ]
                , HE.onClick onclickRegenerate
                ]
                [ HH.text "Regenerate" ]
            ]
        ]
    ]

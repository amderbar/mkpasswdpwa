module Component.Form.Dropdown where

import Prelude

import Component.RenderUtil (classes)
import Effect.Aff.Class (class MonadAff)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Halogen.HTML.Properties.ARIA as ARIA
import Type.Proxy (Proxy(..))
import Web.Event.Event as Web
import Web.UIEvent.MouseEvent (MouseEvent, toEvent)

slot
  :: forall
       (query :: Type -> Type)
       (action :: Type)
       (m :: Type -> Type)
       (slot :: Type)
       (output :: Type)
       (row :: Row Type)
   . Ord slot
  => MonadAff m
  => slot
  -> Input output
  -> (output -> action)
  -> H.ComponentHTML action ("Dropdown" :: H.Slot query output slot | row) m
slot id input = HH.slot (Proxy :: Proxy "Dropdown") id component input

slot_
  :: forall (query :: Type -> Type) (action :: Type) (m :: Type -> Type) (slot :: Type) (output :: Type) (row :: Row Type)
   . Ord slot
  => MonadAff m
  => slot
  -> Input output
  -> H.ComponentHTML action ("Dropdown" :: H.Slot query output slot | row) m
slot_ id input = HH.slot_ (Proxy :: Proxy "Dropdown") id component input

type Input out = State out

data Action o
  = Activate Boolean
  | Change (MouseEvent -> o) MouseEvent

data Contents out
  = Divider
  | Item
      { onClick :: MouseEvent -> out
      , label :: String
      }

type State out =
  { isActive :: Boolean
  , label :: String
  , contents :: Array (Contents out)
  }

component :: forall q o m. MonadAff m => H.Component q (State o) o m
component =
  H.mkComponent
    { initialState: identity
    , render
    , eval: H.mkEval $ H.defaultEval { handleAction = handleAction }
    }
  where
  render :: State o -> H.ComponentHTML _ _ _
  render { isActive, label, contents } =
    HH.div
      [ classes $ [ "dropdown" ] <> if isActive then [ "is-active" ] else [] ]
      [ HH.div
          [ classes [ "dropdown-trigger" ]
          ]
          [ HH.button
              [ classes [ "button" ]
              , ARIA.hasPopup "true"
              , ARIA.controls "dropdown-menu"
              , HE.onClick \_ -> Activate (not isActive)
              ]
              [ HH.span_ [ HH.text label ]
              , HH.span
                  [ classes [ "icon", "is-small" ] ]
                  [ HH.i
                      [ classes [ "fas", "fa-angle-down" ]
                      , ARIA.hidden "true"
                      ]
                      []
                  ]
              ]
          ]
      , HH.div
          [ classes [ "dropdown-menu" ], ARIA.role "menu" ]
          [ HH.div
              [ classes [ "dropdown-content" ] ]
              (mkDropdownContents <$> contents)
          ]
      ]

  mkDropdownContents = case _ of
    Divider -> HH.hr [ classes [ "dropdown-divider" ] ]
    Item conf -> dropdownItem conf

  dropdownItem { onClick, label } =
    HH.a
      [ classes [ "dropdown-item" ]
      , HP.href "#"
      , HE.onClick (Change onClick)
      ]
      [ HH.text label ]

  handleAction :: Action o -> H.HalogenM _ _ _ _ _ Unit
  handleAction = case _ of
    Activate flg -> H.modify_ _ { isActive = flg }
    Change onClick ev -> do
      H.liftEffect $ Web.preventDefault (toEvent ev)
      H.raise (onClick ev)
      handleAction (Activate false)

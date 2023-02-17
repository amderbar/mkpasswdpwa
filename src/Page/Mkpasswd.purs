module Page.Mkpasswd
  ( Slot
  , component
  ) where

import Prelude

import Component.Form.InputLength as InputLength
import Component.Form.InputRequiedChars as InputRequiedChars
import Component.HeaderNav as Nav
import Component.RenderUtil (classes, footerBtnArea, resultModal)
import Control.Monad.Maybe.Trans (MaybeT(..), runMaybeT)
import Data.Length (fromLength)
import Data.Maybe (Maybe(..), fromMaybe, maybe)
import Data.Newtype (unwrap)
import Data.Passwd (Passwd)
import Data.Passwd.Gen (genPasswd)
import Data.Policy (defaultPolicy)
import Effect.Aff.Class (class MonadAff)
import Halogen as H
import Halogen.HTML as HH
import Test.QuickCheck.Gen (randomSampleOne)
import Type.Proxy (Proxy(..))

type Slot id
  = forall q. H.Slot q Void id

type ChildSlots
  = ( headerNav :: Nav.Slot Unit
    )

slots :: { _headerNav :: Proxy "headerNav" }
slots =
  { _headerNav: Proxy :: Proxy "headerNav" }

type State
  = { passwd :: Maybe Passwd
    }

type Input
  = Unit

type Output
  = Maybe Passwd

data Action
  = Generate
  | Clear
  | Save

component :: forall q m. MonadAff m => H.Component q Input Output m
component =
  H.mkComponent
    { initialState: const { passwd: Nothing }
    , render
    , eval: H.mkEval $ H.defaultEval { handleAction = handleAction }
    }
  where
  render :: State -> H.ComponentHTML _ _ _
  render state =
    HH.main
      [ classes $ maybe [] (const ["is-clipped"]) state.passwd ]
      [ HH.slot slots._headerNav unit Nav.component unit absurd
      , formArea
      , footerBtnArea (const Generate)
      , resultModal
          { onclickClose: const Clear
          , onclickSave: const Save
          , onclickRegenerate: const Generate
          }
          (unwrap <$> state.passwd)
      ]

  formArea :: H.ComponentHTML _ _ _
  formArea =
    HH.section
      [ classes [ "section" ] ]
      [ HH.div
          [ classes [ "container" ] ]
          [ InputLength.slot_ unit (fromLength defaultPolicy.length)
          , InputRequiedChars.slot_ unit (InputRequiedChars.mkInput defaultPolicy.required)
          ]
      ]

  handleAction :: Action -> H.HalogenM _ _ _ _ _ Unit
  handleAction = case _ of
    Clear -> H.modify_ _ { passwd = Nothing }
    Generate -> runMaybeT handleGenerate >>= (pure <<< fromMaybe unit)
    Save -> H.raise =<< H.gets _.passwd

  handleGenerate :: MaybeT (H.HalogenM _ _ _ _ _) Unit
  handleGenerate = do
    length <- MaybeT $ H.request InputLength.proxy unit InputLength.GetResult
    required <- MaybeT $ H.request InputRequiedChars.proxy unit InputRequiedChars.GetResult
    newPasswd <- H.liftEffect $ randomSampleOne (genPasswd { length, required })
    H.modify_ _ { passwd = Just newPasswd }

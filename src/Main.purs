module Main where

import Prelude
import Mkpasswd                  (mkpasswd, defaultPolicy)
import Data.Maybe                (Maybe(..))
import Data.Either               (Either(..), either)
import Effect                    (Effect)
import Effect.Aff                (Aff)
import Halogen             as H
import Halogen.Aff         as HA
import Halogen.HTML        as HH
import Halogen.HTML.Events as HE
import Halogen.VDom.Driver       (runUI)

type State = Either String String

data Query a = Regenerate a

ui :: H.Component HH.HTML Query Unit Void Aff
ui =
  H.component
    { initialState: const initialState
    , render
    , eval: \(Regenerate next) -> do
          newPasswd <- H.liftEffect $ mkpasswd defaultPolicy
          H.put newPasswd
          pure next
    , receiver : const Nothing
    }
    where
          initialState :: State
          initialState = Left "No password generated yet"

          --render :: forall m. State -> H.ComponentHTML Query () m
          --render :: forall m. State -> H.ComponentHTML (Query Unit)
          render state =
            let
              value = either show show state
            in
              HH.div_ $
                [ HH.h1_ [ HH.text "Mkpasswd" ]
                , HH.p_ [ HH.text ("Current value: " <> value) ]
                , HH.button
                    [ HE.onClick (HE.input_ Regenerate) ]
                    [ HH.text "Generate new Password" ]
                ]

main :: Effect Unit
main = HA.runHalogenAff do
  body <- HA.awaitBody
  runUI ui unit body

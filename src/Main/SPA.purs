module Main.SPA (main) where

import Prelude

import Component.Router (Query(ChangeHash), rootComponent)
import Effect (Effect)
import Effect.Routing (routing)
import Halogen as H
import Halogen.Aff as HA
import Halogen.VDom.Driver (runUI)

main :: Effect Unit
main =
  HA.runHalogenAff do
    body <- HA.awaitBody
    app <- runUI rootComponent unit body
    routing (app.query <<< H.mkTell <<< ChangeHash)

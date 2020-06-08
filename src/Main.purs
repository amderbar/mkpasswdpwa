module Main where

import Prelude
import Effect                    (Effect)
import Halogen.Aff             as HA
import Halogen.VDom.Driver       (runUI)
import Mkpasswd.UI               (component, Query(ChangeHash), routing)

main :: Effect Unit
main = HA.runHalogenAff do
  body <- HA.awaitBody
  app <- runUI component unit body
  routing app.query ChangeHash

module Main where

import Prelude
import Mkpasswd.Routing          (routing)
import Mkpasswd.Component        (ui, Query(..))
import Effect                    (Effect)
import Halogen.Aff             as HA
import Halogen.VDom.Driver       (runUI)

main :: Effect Unit
main = HA.runHalogenAff do
  body <- HA.awaitBody
  app <- runUI ui unit body
  routing app.query ChangeHash

module Main where

import Prelude
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Effect.Aff (Aff, launchAff_)
import Effect.Class (liftEffect)
import Halogen as H
import Halogen.Aff as HA
import Halogen.VDom.Driver (runUI)
import Mkpasswd.Data.Routing (RouteHash, menuHash)
import Mkpasswd.UI (component, Query(ChangeHash))
import Routing.Hash (matches)

main :: Effect Unit
main =
  HA.runHalogenAff do
    body <- HA.awaitBody
    app <- runUI component unit body
    liftEffect (routing app.query)
  where
  routing :: forall t. (Query Unit -> Aff t) -> Effect (Effect Unit)
  routing query = matches menuHash (handleMatches query)

  handleMatches ∷ forall t. (Query Unit -> Aff t) -> Maybe RouteHash -> RouteHash -> Effect Unit
  handleMatches query mOld new =
    when (mOld /= Just new)
      $ launchAff_
      $ query
      $ H.mkTell (ChangeHash new)

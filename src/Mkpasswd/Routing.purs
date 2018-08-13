module Mkpasswd.Routing where

import Prelude
import Data.Foldable             (oneOf)
import Data.Generic.Rep          (class Generic)
import Data.Generic.Rep.Show     (genericShow)
import Effect                    (Effect)
import Effect.Aff                (Aff, launchAff_)
import Effect.Class              (liftEffect)
import Halogen                 as H
import Routing.Hash              (matches)
import Routing.Match             (Match, lit, end)

data RouteHash
    = LinkA
    | LinkB

derive instance genericRouteHash :: Generic RouteHash _
instance showRouteHash :: Show RouteHash where
    show = genericShow

menuHash :: Match RouteHash
menuHash = lit "link" *> oneOf
    [ LinkA <$ lit "a"
    , LinkB <$ lit "b"
    ] <* end

routing :: forall m a. (m Unit -> Aff a) -> (RouteHash -> Unit -> m Unit) -> Aff (Effect Unit)
routing query hashAction = liftEffect $ matches menuHash \_ newHash ->
      launchAff_ $ query $ H.action $ hashAction newHash

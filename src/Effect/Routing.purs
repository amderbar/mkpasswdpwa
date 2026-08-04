module Effect.Routing
  ( RouteHash(..)
  , hashStr
  , menuHash
  , routing
  ) where

import Prelude

import Data.Foldable (oneOf)
import Effect (Effect)
import Effect.Aff (Aff, launchAff_)
import Effect.Class (liftEffect)
import Routing.Hash (matches)
import Routing.Match (Match, lit, end)

data RouteHash
  = Index
  | List

derive instance eqRouteHash :: Eq RouteHash

instance showRouteHash :: Show RouteHash where
  show = case _ of
    Index -> ""
    List -> "list"

hashStr :: RouteHash -> String
hashStr = ("#" <> _) <<< show

menuHash :: Match RouteHash
menuHash =
  oneOf
    [ List <$ lit "list"
    , pure Index
    ]
    <* end

routing :: forall t. (RouteHash -> Aff t) -> Aff (Effect Unit)
routing query = liftEffect $ matches menuHash handleMatches
  where
  handleMatches ∷ Maybe RouteHash -> RouteHash -> Effect Unit
  handleMatches mOld new =
    when (mOld /= Just new)
      $ launchAff_
      $ void
      $ query new

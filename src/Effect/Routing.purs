module Effect.Routing
  ( RouteHash(..)
  , forcusIdx
  , hashStr
  , menuHash
  , routing
  )
  where

import Prelude

import Data.Foldable (oneOf)
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Effect.Aff (Aff, launchAff_)
import Effect.Class (liftEffect)
import Routing.Hash (matches)
import Routing.Match (Match, lit, int, end)

data RouteHash
  = Index
  | List
  | New
  | Store Int

derive instance eqRouteHash :: Eq RouteHash

instance showRouteHash :: Show RouteHash where
  show = case _ of
    Index -> ""
    List -> "list"
    New -> "new"
    (Store i) -> "store/" <> (show i)

hashStr :: RouteHash -> String
hashStr = ("#" <> _) <<< show

menuHash :: Match RouteHash
menuHash =
  oneOf
    [ Store <$> (lit "store" *> int)
    , New <$ lit "new"
    , List <$ lit "list"
    , pure Index
    ]
    <* end

forcusIdx :: RouteHash -> Maybe Int
forcusIdx = case _ of
  Store i -> Just i
  _ -> Nothing

routing :: forall t. (RouteHash -> Aff t) -> Aff (Effect Unit)
routing query = liftEffect $ matches menuHash handleMatches
  where
  handleMatches ∷ Maybe RouteHash -> RouteHash -> Effect Unit
  handleMatches mOld new =
    when (mOld /= Just new)
      $ launchAff_
      $ void
      $ query new

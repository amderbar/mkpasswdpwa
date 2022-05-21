module Data.Routing
  ( RouteHash(..)
  , forcusIdx
  , hashStr
  , menuHash
  ) where

import Data.Foldable (oneOf)
import Data.Maybe (Maybe(..))
import Prelude (class Eq, class Show, show, pure, (<>), (<<<), (<*), (*>), (<$>), (<$))
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

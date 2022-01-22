module Mkpasswd.Data.Routing
  ( RouteHash(..)
  , hashStr
  , menuHash
  )
  where

import Prelude
  ( class Eq
  , class Show
  , show
  , pure
  , (<>)
  , (<<<)
  , (<*)
  , (*>)
  , (<$>)
  , (<$)
  )
import Data.Foldable (oneOf)
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

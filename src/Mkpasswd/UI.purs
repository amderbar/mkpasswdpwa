module Mkpasswd.UI
  ( component
  , Query(..)
  , module Mkpasswd.UI.Routing
  ) where

import Prelude
import Data.Maybe            (Maybe(..))
import Halogen             as H
import Halogen.HTML        as HH
import Mkpasswd.UI.Routing   (RouteHash(..), routing)

type State =
  { route :: Maybe RouteHash
  }

data Query a = ChangeHash RouteHash a

component :: forall i o m. H.Component HH.HTML Query i o m
component =
  H.mkComponent
    { initialState
    , render
    , eval: H.mkEval $ H.defaultEval { handleQuery = handleQuery }
    }
  where
  initialState _ =
    { route : Just Index
    }

  render state =
    HH.div_
      [ HH.h1_ [ HH.text (show state.route) ]
      ]

  handleQuery :: forall a u. Query u -> H.HalogenM State a () o m (Maybe u)
  handleQuery = case _ of
    ChangeHash route a -> do
      mRoute <- H.gets _.route
      when ( mRoute /= Just route ) $
        H.modify_ _ { route = Just route }
      pure ( Just a )

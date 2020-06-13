module Mkpasswd.UI
  ( component
  , Query(..)
  , module Mkpasswd.UI.Routing
  ) where

import Prelude
import Data.Array            (deleteAt)
import Data.Either           (Either(..))
import Data.List             (List(Nil))
import Data.List.Types       (toList)
import Data.Maybe            (Maybe(..), maybe)
import Data.Symbol           (SProxy(..))
import Effect.Class          (class MonadEffect)
import Effect.Storage        (fetch, save)
import Foreign               (ForeignError)
import Halogen             as H
import Halogen.HTML        as HH
import Mkpasswd.Data.States  (FormData)
import Mkpasswd.UI.Pages.List as ListPage
import Mkpasswd.UI.Routing   (RouteHash(..), routing)

type Slots = ( listPage :: ListPage.Slot Unit )

_listPage = SProxy :: SProxy "listPage"

type State =
  { route :: RouteHash
  , storage :: Array FormData
  , error  :: List ForeignError
  }

data Action
    = Load
    | Delete ListPage.Message

data Query a = ChangeHash RouteHash a

component :: forall i o m. MonadEffect m => H.Component HH.HTML Query i o m
component =
  H.mkComponent
    { initialState
    , render
    , eval: H.mkEval $ H.defaultEval
      { handleAction = handleAction
      , handleQuery = handleQuery
      , initialize = Just Load
      }
    }
  where
  initialState _ =
    { route : Index
    , storage : []
    , error  : Nil
    }

  render {route, storage} =
    case route of
      List    -> HH.slot _listPage unit ListPage.component storage (Just <<< Delete)
      _       -> HH.div_ [HH.h1_ [ HH.text (show route) ]]

  wsKey :: String
  wsKey = "mkpasswd"

  handleAction :: Action -> H.HalogenM State Action Slots o m Unit
  handleAction = case _ of
    Load -> do
      ns <- H.liftEffect $ fetch wsKey
      case ns of
        Right fd -> H.modify_ _ { storage = (fd :: Array FormData) }
        Left  er -> H.modify_ _ { error   = toList er }

    Delete (ListPage.DelPasswd i) -> do
      s <- H.get
      let newSt = deleteAt i s.storage
      newSt # maybe (pure unit) \st -> do
        H.modify_ _ { storage = st }
        H.liftEffect $ save wsKey st

  handleQuery :: forall a u. Query u -> H.HalogenM State a Slots o m (Maybe u)
  handleQuery = case _ of
    ChangeHash route a -> do
      mRoute <- H.gets _.route
      when ( mRoute /= route ) $ H.modify_ _ { route = route }
      pure ( Just a )

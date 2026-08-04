module Component.Router where

import Prelude

import Data.Array (deleteAt)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Effect.Routing (RouteHash(..))
import Data.States (FormData)
import Effect.Aff.Class (class MonadAff)
import Effect.Console as Console
import Effect.Storage (fetch, save)
import Halogen as H
import Halogen.HTML as HH
import Page.List as ListPage
import Page.Mkpasswd as MkpasswdPage
import Type.Proxy (Proxy(..))

type State =
  { route :: RouteHash
  , storage :: Array FormData
  }

data Action
  = Load
  | Delete ListPage.DeleteTargetIdx

data Query a = ChangeHash RouteHash a

type Slots =
  ( mkpasswdPage :: MkpasswdPage.Slot Unit
  , listPage :: ListPage.Slot Unit
  )

_mkpasswdPage = Proxy :: Proxy "mkpasswdPage"

_listPage = Proxy :: Proxy "listPage"

rootComponent :: forall i o m. MonadAff m => H.Component Query i o m
rootComponent =
  H.mkComponent
    { initialState
    , render
    , eval:
        H.mkEval
          $ H.defaultEval
              { handleAction = handleAction
              , handleQuery = handleQuery
              , initialize = Just Load
              }
    }
  where
  initialState _ = { route: Index, storage: [] }

  render :: State -> H.ComponentHTML _ _ _
  render { route, storage } = case route of
    Index -> HH.slot_ _mkpasswdPage unit MkpasswdPage.component unit
    List -> HH.slot _listPage unit ListPage.component storage Delete

  wsKey :: String
  wsKey = "mkpasswd"

  handleAction :: Action -> H.HalogenM _ _ _ _ _ Unit
  handleAction = case _ of
    Load -> do
      ns <- H.liftEffect $ fetch wsKey
      case ns of
        Right fd -> H.modify_ _ { storage = (fd :: Array FormData) }
        Left er -> H.liftEffect $ Console.logShow er
    Delete i -> do
      s <- H.gets _.storage
      let
        newSt = deleteAt i s
      case newSt of
        Nothing -> pure unit
        Just st -> do
          H.modify_ _ { storage = st }
          H.liftEffect $ save wsKey st

  handleQuery :: forall u. Query u -> H.HalogenM _ _ _ _ _ (Maybe u)
  handleQuery = case _ of
    ChangeHash route a -> do
      mRoute <- H.gets _.route
      when (mRoute /= route) $ H.modify_ _ { route = route }
      pure (Just a)

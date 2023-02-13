module Component.Router where

import Prelude

import Data.Array (deleteAt, snoc, updateAt, (!!))
import Data.Either (Either(..))
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Newtype (unwrap)
import Data.Passwd (Passwd)
import Effect.Routing (RouteHash(..), forcusIdx)
import Data.States (FormData, initialForm)
import Effect.Aff.Class (class MonadAff)
import Effect.Console as Console
import Effect.Storage (fetch, save)
import Halogen as H
import Halogen.HTML as HH
import Page.List as ListPage
import Page.Mkpasswd as MkpasswdPage
import Page.Store as StorePage
import Type.Proxy (Proxy(..))

type State
  = { route :: RouteHash
    , session :: Maybe Passwd
    , storage :: Array FormData
    }

data Action
  = Load
  | Mkpasswd (Maybe Passwd)
  | Save StorePage.Output
  | Delete ListPage.DeleteTargetIdx

data Query a
  = ChangeHash RouteHash a

type Slots
  = ( mkpasswdPage :: MkpasswdPage.Slot Unit
    , listPage :: ListPage.Slot Unit
    , storePage :: StorePage.Slot Unit
    )

_mkpasswdPage = Proxy :: Proxy "mkpasswdPage"

_listPage = Proxy :: Proxy "listPage"

_storePage = Proxy :: Proxy "storePage"

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
  initialState _ = { route: Index, session: Nothing, storage: [] }

  render :: State -> H.ComponentHTML _ _ _
  render { route, session, storage } = case route of
    Index -> HH.slot _mkpasswdPage unit MkpasswdPage.component unit Mkpasswd
    List -> HH.slot _listPage unit ListPage.component storage Delete
    New ->
      let
        initialValues = initialForm { passwd = _ } <<< unwrap <$> session
      in
        HH.slot _storePage unit (StorePage.component initialValues) unit Save
    Store i -> HH.slot _storePage unit (StorePage.component $ storage !! i) unit Save

  wsKey :: String
  wsKey = "mkpasswd"

  handleAction :: Action -> H.HalogenM _ _ _ _ _ Unit
  handleAction = case _ of
    Load -> do
      ns <- H.liftEffect $ fetch wsKey
      case ns of
        Right fd -> H.modify_ _ { storage = (fd :: Array FormData) }
        Left er -> H.liftEffect $ Console.logShow er
    Mkpasswd p -> H.modify_ _ { session = p }
    Save fd -> do
      r <- H.gets _.route
      s <- H.gets _.storage
      let
        st = fromMaybe (snoc s fd) $ (\i -> updateAt i fd s) =<< forcusIdx r
      H.modify_ _ { storage = st }
      H.liftEffect $ save wsKey st
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
      when (route == List) $ H.modify_ (_ { session = Nothing })
      pure (Just a)

module Mkpasswd.UI
  ( component
  , Query(..)
  ) where

import Prelude
import Data.Array (snoc, (!!), deleteAt, updateAt)
import Data.Either (Either(..))
import Data.List (List(Nil))
import Data.List.Types (toList)
import Data.Maybe (Maybe(..), maybe, fromMaybe)
import Type.Proxy (Proxy(..))
import Effect.Aff.Class (class MonadAff)
import Effect.Storage (fetch, save)
import Foreign (ForeignError)
import Halogen as H
import Halogen.HTML as HH
import Mkpasswd.Data.States (FormData, initialForm)
import Mkpasswd.UI.Pages.List as ListPage
import Mkpasswd.UI.Pages.Mkpasswd as IndexPage
import Mkpasswd.UI.Pages.Store as StorePage
import Mkpasswd.Data.Routing (RouteHash(..))

type Slots
  = ( listPage :: ListPage.Slot Unit
    , storePage :: StorePage.Slot Unit
    , indexPage :: IndexPage.Slot Unit
    )

_listPage = Proxy :: Proxy "listPage"

_storePage = Proxy :: Proxy "storePage"

_indexPage = Proxy :: Proxy "indexPage"

type State
  = { route :: RouteHash
    , storage :: Array FormData
    , passwd :: Maybe String
    , error :: List ForeignError
    }

data Action
  = Load
  | Save FormData
  | Delete Int
  | Mkpasswd (Maybe String)

data Query a
  = ChangeHash RouteHash a

component :: forall i o m. MonadAff m => H.Component Query i o m
component =
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
  initialState _ =
    { route: Index
    , storage: []
    , passwd: Nothing
    , error: Nil
    }

  render :: State -> H.ComponentHTML Action Slots m
  render { route, storage, passwd } = case route of
    Index -> HH.slot _indexPage unit IndexPage.component unit Mkpasswd
    List -> HH.slot _listPage unit ListPage.component storage Delete
    New -> HH.slot _storePage unit StorePage.component (initialForm { passwd = _ } <$> passwd) Save
    Store i -> HH.slot _storePage unit StorePage.component (storage !! i) Save

  wsKey :: String
  wsKey = "mkpasswd"

  handleAction :: Action -> H.HalogenM State Action Slots o m Unit
  handleAction = case _ of
    Load -> do
      ns <- H.liftEffect $ fetch wsKey
      case ns of
        Right fd -> H.modify_ _ { storage = (fd :: Array FormData) }
        Left er -> H.modify_ _ { error = toList er }
    Mkpasswd p -> do
      H.modify_ _ { passwd = p }
    Save fd -> do
      s <- H.get
      let
        st = fromMaybe (snoc s.storage fd) $ (\i -> updateAt i fd s.storage) =<< (forcusIdx s.route)
      H.modify_ _ { storage = st }
      H.liftEffect $ save wsKey st
      where
      forcusIdx :: RouteHash -> Maybe Int
      forcusIdx = case _ of
        Store i -> Just i
        _ -> Nothing
    Delete i -> do
      s <- H.get
      let
        newSt = deleteAt i s.storage
      newSt
        # maybe (pure unit) \st -> do
            H.modify_ _ { storage = st }
            H.liftEffect $ save wsKey st

  handleQuery :: forall a u. Query u -> H.HalogenM State a Slots o m (Maybe u)
  handleQuery = case _ of
    ChangeHash route a -> do
      mRoute <- H.gets _.route
      when (mRoute /= route) $ H.modify_ _ { route = route }
      when (route == List) $ H.modify_ (_ { passwd = Nothing })
      pure (Just a)

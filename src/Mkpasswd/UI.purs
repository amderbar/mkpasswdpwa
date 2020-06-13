module Mkpasswd.UI
  ( component
  , Query(..)
  , module Mkpasswd.UI.Routing
  ) where

import Prelude
import Data.Array            (snoc, (!!), deleteAt, updateAt)
import Data.Either           (Either(..))
import Data.List             (List(Nil))
import Data.List.Types       (toList)
import Data.Maybe            (Maybe(..), maybe, fromMaybe)
import Data.Symbol           (SProxy(..))
import Effect.Aff.Class      (class MonadAff)
import Effect.Storage        (fetch, save)
import Foreign               (ForeignError)
import Halogen             as H
import Halogen.HTML        as HH
import Mkpasswd.Data.States  (FormData, initialForm)
import Mkpasswd.UI.Pages.List as ListPage
import Mkpasswd.UI.Pages.Store as StorePage
import Mkpasswd.UI.Routing   (RouteHash(..), routing)

type Slots =
  ( listPage :: ListPage.Slot Unit
  , storePage :: StorePage.Slot Unit
  )

_listPage = SProxy :: SProxy "listPage"
_storePage = SProxy :: SProxy "storePage"

type State =
  { route :: RouteHash
  , storage :: Array FormData
  , passwd :: Maybe String
  , error  :: List ForeignError
  }

data Action
    = Load
    | Save StorePage.Message
    | Delete ListPage.Message

data Query a = ChangeHash RouteHash a

component :: forall i o m. MonadAff m => H.Component HH.HTML Query i o m
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
    , passwd : Nothing
    , error  : Nil
    }

  render {route, storage, passwd} =
    case route of
      -- Index   -> HH.slot cpMkpasswd unit Mk.ui unit (HE.input Mkpasswd)
      Index   -> HH.div_ [HH.h1_ [ HH.text (show route) ]]
      List    -> HH.slot _listPage unit ListPage.component storage (Just <<< Delete)
      New     -> HH.slot _storePage unit StorePage.component (initialForm { passwd = _ } <$> passwd) (Just <<< Save)
      Store i -> HH.slot _storePage unit StorePage.component (storage !! i) (Just <<< Save)

  wsKey :: String
  wsKey = "mkpasswd"

  handleAction :: Action -> H.HalogenM State Action Slots o m Unit
  handleAction = case _ of
    Load -> do
      ns <- H.liftEffect $ fetch wsKey
      case ns of
        Right fd -> H.modify_ _ { storage = (fd :: Array FormData) }
        Left  er -> H.modify_ _ { error   = toList er }

    Save (StorePage.SavePasswd fd) -> do
      s <- H.get
      let st = fromMaybe (snoc s.storage fd) $ (\i -> updateAt i fd s.storage) =<< (forcusIdx s.route)
      H.modify_ _ { storage = st }
      H.liftEffect $ save wsKey st
      where
      forcusIdx :: RouteHash -> Maybe Int
      forcusIdx = case _ of
        Store i   -> Just i
        otherwise -> Nothing

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

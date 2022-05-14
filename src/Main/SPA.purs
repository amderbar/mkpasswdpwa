module Main.SPA (main) where

import Prelude
import Data.Maybe (Maybe(..))
import Data.Passwd (Passwd)
import Data.Routing (RouteHash(..), menuHash)
import Effect (Effect)
import Effect.Aff (Aff, launchAff_)
import Effect.Aff.Class (class MonadAff)
import Halogen as H
import Halogen.Aff as HA
import Halogen.HTML as HH
import Halogen.VDom.Driver (runUI)
import Page.Mkpasswd as MkpasswdPage
import Routing.Hash (matches)
import Type.Proxy (Proxy(..))

main :: Effect Unit
main =
  HA.runHalogenAff do
    body <- HA.awaitBody
    app <- runUI rootComponent unit body
    H.liftEffect (routing app.query)
  where
  routing :: forall t. (Query Unit -> Aff t) -> Effect (Effect Unit)
  routing query = matches menuHash (handleMatches query)

  handleMatches ∷ forall t. (Query Unit -> Aff t) -> Maybe RouteHash -> RouteHash -> Effect Unit
  handleMatches query mOld new =
    when (mOld /= Just new)
      $ launchAff_
      $ void
      $ query
      $ H.mkTell (ChangeHash new)

type State
  = { route :: RouteHash }

data Action
  = Mkpasswd (Maybe Passwd)

data Query a
  = ChangeHash RouteHash a

type Slots
  = ( mkpasswdPage :: MkpasswdPage.Slot Unit
    )

_mkpasswdPage = Proxy :: Proxy "mkpasswdPage"

rootComponent :: forall i o m. MonadAff m => H.Component Query i o m
rootComponent =
  H.mkComponent
    { initialState
    , render
    , eval: H.mkEval $ H.defaultEval { handleAction = handleAction, handleQuery = handleQuery }
    }
  where
  initialState _ = { route: Index }

  render :: State -> H.ComponentHTML _ _ _
  render s@{ route } = case route of
    Index -> HH.slot _mkpasswdPage unit MkpasswdPage.component unit Mkpasswd
    _ -> render s { route = Index }

  handleAction :: Action -> H.HalogenM _ _ _ _ _ Unit
  handleAction = case _ of
    Mkpasswd _ -> pure unit

  handleQuery :: forall u. Query u -> H.HalogenM _ _ _ _ _ (Maybe u)
  handleQuery = case _ of
    ChangeHash route a -> do
      mRoute <- H.gets _.route
      when (mRoute /= route) $ H.modify_ _ { route = route }
      pure (Just a)

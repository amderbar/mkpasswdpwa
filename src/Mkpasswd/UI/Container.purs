module Mkpasswd.UI.Container where

import Prelude
import Mkpasswd.UI.Pages.Mkpasswd as Mk
import Mkpasswd.UI.Pages.List     as Lt
import Mkpasswd.UI.Pages.Store    as St
import Mkpasswd.UI.Routing         (RouteHash(..))
import Mkpasswd.Data.Array         (updateAt)
import Mkpasswd.Data.States        (FormData, initialForm)
import Mkpasswd.Data.Storage       (fetch, save)
import Data.Array                  (snoc, (!!), deleteAt)
import Data.Const                  (Const)
import Data.Either                 (Either(..))
import Data.List                   (List(..))
import Data.List.Types             (toList)
import Data.Maybe                  (Maybe(..), isJust, fromMaybe, maybe)
import Effect.Aff                  (Aff)
import Foreign                     (ForeignError)
--import Foreign                     (ForeignError, renderForeignError)
import Halogen                     as H
import Halogen.Component.ChildPath as HC
import Halogen.Data.Prism          (type (<\/>), type (\/))
import Halogen.HTML                as HH
import Halogen.HTML.Events         as HE

type ChildQuery = Mk.Query <\/> Lt.Query <\/> St.Query <\/> Const Void
type Slot  = Unit \/ Unit \/ Unit \/ Void

cpMkpasswd :: HC.ChildPath Mk.Query ChildQuery Unit Slot
cpMkpasswd = HC.cp1

cpList :: HC.ChildPath Lt.Query ChildQuery Unit Slot
cpList = HC.cp2

cpStore :: HC.ChildPath St.Query ChildQuery Unit Slot
cpStore = HC.cp3

type State =
    { route  :: RouteHash
    , storage:: Array FormData
    , passwd :: Maybe String
    , error  :: List ForeignError
    }

data Query a
    = ChangeHash RouteHash a
    | Mkpasswd (Maybe String) a
    | Load a
    | Save St.Message a
    | Delete Lt.Message a

wsKey :: String
wsKey = "mkpasswd"

ui :: H.Component HH.HTML Query Unit Void Aff
ui =
  H.lifecycleParentComponent
    { initialState: const initialState
    , render
    , eval
    , receiver : const Nothing
    , initializer: Just (H.action Load)
    , finalizer  : Nothing
    }
    where
          initialState :: State
          initialState =
              { route  : Index
              , storage: []
              , passwd : Nothing
              , error  : Nil
              }

          render :: State -> H.ParentHTML Query ChildQuery Slot Aff
          render state =
              case state.route of
                Index   ->  HH.slot' cpMkpasswd unit Mk.ui unit (HE.input Mkpasswd)
                List    ->  HH.slot' cpList unit Lt.ui state.storage (HE.input Delete)
                New     ->  HH.slot' cpStore unit St.ui (initialForm { passwd = _ } <$> state.passwd) (HE.input Save)
                Store i ->  HH.slot' cpStore unit St.ui (state.storage !! i) (HE.input Save)

          eval :: Query ~> H.ParentDSL State Query ChildQuery Slot Void Aff
          eval (ChangeHash newHash next) = do
             H.modify_ (_ { route = newHash })
             when (newHash == List) $ H.modify_ (_ { passwd = Nothing })
             pure next
          eval (Mkpasswd p next) = do
             H.modify_ (_ { passwd = p })
             pure next
          eval (Load next) = do
             ns <- H.liftEffect $ fetch wsKey
             case ns of
                  Right fd -> H.modify_ (_ { storage = (fd :: Array FormData) })
                  Left  er -> H.modify_ (_ { error   = toList er })
             pure next
          eval (Save (St.SavePasswd fd) next) = do
             s <- H.get
             let idx = case s.route of
                    Store i   -> Just i
                    otherwise -> Nothing
             let st = if isJust $ (s.storage !! _) =<< idx
                    then updateAt (fromMaybe 0 idx) fd s.storage
                    else snoc s.storage fd
             H.modify_ (_ { storage = st })
             H.liftEffect $ save wsKey st
             pure next
          eval (Delete (Lt.DelPasswd i) next) = do
             s <- H.get
             let newSt = deleteAt i s.storage
             newSt # maybe (pure unit) \st -> do
                H.modify_ (_ { storage = st })
                H.liftEffect $ save wsKey st
             pure next

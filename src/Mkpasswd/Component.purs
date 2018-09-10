module Mkpasswd.Component where

import Prelude
import Mkpasswd.Halogen.Util       (classes, style)
import Mkpasswd.Component.Mkpasswd as Mk
import Mkpasswd.Component.List     as Lt
import Mkpasswd.Component.Store    as St
import Mkpasswd.Data.Array         (updateAt)
import Mkpasswd.Data.States        (FormData)
import Mkpasswd.Data.Storage       (fetch, save)
import Mkpasswd.Routing            (RouteHash(..))
import Data.Array                  (snoc, (!!))
import Data.Const                  (Const)
import Data.Either                 (Either(..))
import Data.List                   (List(..))
import Data.List.Types             (toList)
import Data.Maybe                  (Maybe(..), isJust, fromMaybe)
import Effect.Aff                  (Aff)
import Effect.Class                (liftEffect)
import Foreign                     (ForeignError(..), renderForeignError)
import Halogen                     as H
import Halogen.Component.ChildPath as HC
import Halogen.Data.Prism          (type (<\/>), type (\/))
import Halogen.HTML                as HH
import Halogen.HTML.Events         as HE
import Halogen.HTML.Properties     as HP

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
    , error  :: List ForeignError
    }

data Query a
    = ChangeHash RouteHash a
    | Load a
    | Save St.Message a

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
              , error  : Nil
              }

          render :: State -> H.ParentHTML Query ChildQuery Slot Aff
          render state =
              let rt = state.route
               in HH.main
                      [ style "height: 100%"
                      , classes [ "flex" , "flex-column" ]
                      ]
                      [ headerNav
                      , case rt of
                             Index   ->  HH.slot' cpMkpasswd unit Mk.ui unit absurd
                             List    ->  HH.slot' cpList unit Lt.ui state.storage absurd
                             New     ->  HH.slot' cpStore unit St.ui Nothing (HE.input Save)
                             Store i ->  HH.slot' cpStore unit St.ui (state.storage !! i) (HE.input Save)
                      ]
          headerNav =
                HH.nav
                    [ classes [ "border", "flex-none", "flex", "justify-center" ] ]
                    [ HH.a
                        [ classes [ "flex-auto", "border", "p1", "center" ]
                        , HP.href "#"
                        ]
                        [ HH.text "つくる" ]
                    , HH.a
                        [ classes [ "flex-auto", "border", "p1", "center" ]
                        , HP.href "#list"
                        ]
                        [ HH.text "しまう" ]
                    ]

          eval :: Query ~> H.ParentDSL State Query ChildQuery Slot Void Aff
          eval (ChangeHash newHash next) = do
              H.modify_ (_ { route = newHash })
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

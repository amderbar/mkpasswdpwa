module Mkpasswd.Data.Storage where

import Prelude
import Data.Either                 (Either, note)
import Effect                      (Effect)
import Foreign                     (MultipleErrors, ForeignError(..))
import Simple.JSON                 as JSON
import Web.HTML                    as WH
import Web.HTML.Window             as WW
import Web.Storage.Storage         as WS

storage :: WH.Window -> Effect WS.Storage
storage = WW.localStorage

fetch :: forall t. JSON.ReadForeign t => String -> Effect (Either MultipleErrors t)
fetch key = let e = ForeignError $ key <> ": No such key"
             in (note (pure e) >=> JSON.readJSON)
             <$> (WH.window >>= storage >>= WS.getItem key)

save :: forall t. JSON.WriteForeign t => String -> t -> Effect Unit
save key d = WH.window >>= WW.sessionStorage >>= WS.setItem key (JSON.writeJSON d)

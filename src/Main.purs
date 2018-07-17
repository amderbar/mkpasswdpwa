module Main where

import Data.Identity (Identity(..))
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Halogen.Aff.Util (runHalogenAff, awaitBody)
import Halogen.Component (component)
import Halogen.HTML (text)
import Halogen.VDom.Driver (runUI)
import Prelude (const, ($), (>>=), pure, Unit, unit)

main :: Effect Unit
main = runHalogenAff $ awaitBody >>= runUI (component {
    render: const $ text "Hello, World",
    eval: \(Identity a) -> pure a,
    initialState: const unit,
    receiver : const Nothing
}) unit

module Mkpasswd where

import Prelude  (class Show, join, ($), (<$>), (<<<), (=<<))
import Mkpasswd.Data.PasswdPolicy (PasswdPolicy, normalize)
import Mkpasswd.Effect.Random     (choice, shuffle)
import Data.Array            ( replicate, mapMaybe, catMaybes)
import Data.Char             ( fromCharCode )
import Data.Generic.Rep      ( class Generic )
import Data.Generic.Rep.Show ( genericShow )
import Data.String.CodeUnits ( fromCharArray )
import Data.Traversable      ( sequence, traverse )
import Data.Tuple            ( uncurry )
import Data.Unfoldable       ( replicateA )
import Effect                ( Effect )

mkpasswd :: Int -> Array PasswdPolicy -> Effect String
mkpasswd len pol =
    let policy = normalize len pol
     in fromCharCodeArray <$> mkpasscode policy
    where
          fromCharCodeArray = fromCharArray <<< mapMaybe fromCharCode
          multiChoice n arr = replicateA n $ choice arr
          mkpasscode p = shuffle =<< (catMaybes <<< join) <$> traverse (uncurry multiChoice) p

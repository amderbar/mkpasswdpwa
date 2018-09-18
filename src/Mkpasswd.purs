module Mkpasswd where

import Prelude  (class Show, join, ($), (<$>), (<<<), (=<<))
import Mkpasswd.Data.PasswdPolicy (PasswdPolicy, normalize)
import Mkpasswd.Effect.Random     (choice, shuffle)
import Data.Array            ( replicate, mapMaybe, catMaybes)
import Data.Char             ( fromCharCode )
import Data.Generic.Rep      ( class Generic )
import Data.Generic.Rep.Show ( genericShow )
import Data.Maybe            ( Maybe )
import Data.String.CodeUnits ( fromCharArray )
import Data.Traversable      ( sequence, traverse )
import Data.Tuple            ( uncurry )
import Data.Unfoldable       ( replicateA )
import Effect                ( Effect )

mkpasswd :: Int -> Array PasswdPolicy -> Effect (Maybe String)
mkpasswd len pol =
    let policy = normalize len pol
     in (fromCharCodeArray =<< _) <$> mkpasscode policy
    where
          fromCharCodeArray a = fromCharArray <$> traverse fromCharCode a
          multiChoice n arr = replicateA n $ choice arr
          mkpasscode p = (traverse shuffle) =<< (sequence <<< join) <$> traverse (uncurry multiChoice) p

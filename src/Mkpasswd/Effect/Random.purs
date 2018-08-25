module Mkpasswd.Effect.Random where

import Prelude
import Data.Array            ( concat
                             , (..)
                             , null
                             , length
                             , elem
                             , all
                             , (:)
                             , (!!)
                             , deleteAt
                             , replicate
                             , mapMaybe
                             , catMaybes
                             )
import Data.Maybe            ( Maybe(..), fromMaybe )
import Data.Traversable      ( sequence )
import Effect                ( Effect )
import Effect.Random         ( randomInt )

choice :: forall a. Array a -> Effect (Maybe a)
choice []     = pure Nothing
choice popArr =
    let len = length popArr
     in do
        idx <- randomInt 0 (len - 1)
        pure (popArr !! idx)

shuffle :: forall a. Array a -> Effect (Array a)
shuffle target = catMaybes <$> (shuffleInternal [] target)
    where
          shuffleInternal acc []  = pure $ acc
          shuffleInternal acc arr =
             let len = length arr
              in do
                 idx <- randomInt 0 (len - 1)
                 let bcc = (arr !! idx) : acc
                 let brr = fromMaybe [] $ deleteAt idx arr
                 shuffleInternal bcc brr

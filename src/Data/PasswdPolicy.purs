module Data.PasswdPolicy where

type PasswdPolicy =
  { length :: Int
  , required ::
    { degit :: Int
    , lower :: Int
    , upper :: Int
    , symbol :: Int
    }
  }

passwdPolicy :: Int -> Int -> Int -> Int -> Int -> PasswdPolicy
passwdPolicy ln dg lw up sb =
  { length : ln
  , required :
    { degit : dg
    , lower : lw
    , upper : up
    , symbol : sb
    }
  }

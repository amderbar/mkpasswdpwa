{-
Welcome to a Spago project!
You can edit this file as you like.
-}
{ name =
    "mkpasswdpwa"
, dependencies =
    [ "bifunctors"
    , "console"
    , "effect"
    , "generics-rep"
    , "halogen"
    , "prelude"
    , "psci-support"
    , "random"
    , "routing"
    , "simple-json"
    , "validation"
    , "web-html"
    , "web-storage"
    ]
, packages =
    ./packages.dhall
}

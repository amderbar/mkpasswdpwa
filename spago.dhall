{-
Welcome to a Spago project!
You can edit this file as you like.
-}
{ name = "my-project"
, dependencies =
  [ "console"
  , "effect"
  , "foldable-traversable"
  , "gen"
  , "halogen"
  , "psci-support"
  , "quickcheck"
  , "routing"
  , "simple-json"
  , "strings"
  , "tailrec"
  , "web-storage"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}

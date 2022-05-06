{-
Welcome to a Spago project!
You can edit this file as you like.
-}
{ name = "mkpasswdpwa"
, dependencies =
  [ "aff"
  , "argparse-basic"
  , "arrays"
  , "console"
  , "const"
  , "dom-indexed"
  , "effect"
  , "either"
  , "enums"
  , "foldable-traversable"
  , "foreign"
  , "gen"
  , "halogen"
  , "halogen-formless"
  , "integers"
  , "lists"
  , "maybe"
  , "newtype"
  , "node-process"
  , "prelude"
  , "psci-support"
  , "quickcheck"
  , "routing"
  , "strings"
  , "tailrec"
  , "transformers"
  , "tuples"
  , "web-events"
  , "web-html"
  , "web-storage"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}

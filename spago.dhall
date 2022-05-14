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
  , "quickcheck"
  , "routing"
  , "spec"
  , "spec-quickcheck"
  , "strings"
  , "tailrec"
  , "transformers"
  , "tuples"
  , "web-events"
  , "web-html"
  , "web-storage"
  , "web-uievents"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}

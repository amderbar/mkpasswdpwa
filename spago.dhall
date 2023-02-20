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
  , "control"
  , "dom-indexed"
  , "effect"
  , "either"
  , "foldable-traversable"
  , "foreign"
  , "gen"
  , "halogen"
  , "halogen-formless"
  , "integers"
  , "maybe"
  , "newtype"
  , "node-process"
  , "ordered-collections"
  , "partial"
  , "prelude"
  , "quickcheck"
  , "record"
  , "routing"
  , "spec"
  , "spec-quickcheck"
  , "strings"
  , "transformers"
  , "tuples"
  , "unfoldable"
  , "validation"
  , "web-events"
  , "web-html"
  , "web-storage"
  , "web-uievents"
  , "yoga-json"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}

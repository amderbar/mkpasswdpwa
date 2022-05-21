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
  , "prelude"
  , "quickcheck"
  , "routing"
  , "spec"
  , "spec-quickcheck"
  , "strings"
  , "tuples"
  , "web-html"
  , "web-storage"
  , "web-uievents"
  , "yoga-json"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}

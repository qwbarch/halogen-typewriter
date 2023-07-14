{ name = "halogen-typewriter"
, dependencies =
  [ "aff"
  , "css"
  , "datetime"
  , "effect"
  , "foldable-traversable"
  , "halogen"
  , "halogen-css"
  , "halogen-subscriptions"
  , "lists"
  , "maybe"
  , "newtype"
  , "prelude"
  , "profunctor-lenses"
  , "random"
  , "strings"
  , "tailrec"
  , "transformers"
  , "typelevel-prelude"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "example/**/*.purs" ]
}

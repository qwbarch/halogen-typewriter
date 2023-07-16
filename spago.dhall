{ name = "halogen-typewriter"
, dependencies =
  [ "aff"
  , "arrays"
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
  , "substitute"
  , "transformers"
  , "typelevel-prelude"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "example/**/*.purs" ]
}

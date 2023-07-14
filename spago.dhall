{ name = "halogen-typewriter"
, dependencies =
  [ "aff"
  , "datetime"
  , "effect"
  , "foldable-traversable"
  , "halogen"
  , "halogen-subscriptions"
  , "integers"
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

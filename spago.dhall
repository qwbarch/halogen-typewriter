{ name = "halogen-typewriter"
, dependencies =
  [ "aff"
  , "datetime"
  , "effect"
  , "halogen"
  , "integers"
  , "lists"
  , "maybe"
  , "prelude"
  , "profunctor-lenses"
  , "strings"
  , "transformers"
  , "typelevel-prelude"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "example/**/*.purs" ]
}

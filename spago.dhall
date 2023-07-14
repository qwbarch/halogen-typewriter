{ name = "halogen-typewriter"
, dependencies =
  [ "datetime", "effect", "halogen", "lists", "prelude" ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "example/**/*.purs" ]
}

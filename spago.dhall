{ name = "purescript-update-package-sets"
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
, dependencies =
  [ "console", "effect", "psci-support", "simple-ajax", "node-process" ]
}

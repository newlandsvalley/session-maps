{-
Welcome to a Spago project!
You can edit this file as you like.
-}
{ name = "my-project"
, dependencies = [ "affjax"
                 , "argonaut"
                 , "console"
                 , "effect"
                 , "psci-support"
                 , "halogen"
                 , "js-uri"
                 , "openlayers"
                 ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs" ]
}

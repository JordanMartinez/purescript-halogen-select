{-
Welcome to a Spago project!
You can edit this file as you like.
-}
{ name = "halogen-select"
, dependencies =
    [ "affjax"
    , "argonaut"
    , "console"
    , "debug"
    , "effect"
    , "halogen"
    , "halogen-modular"
    , "psci-support"
    , "record"
    , "variant"
    ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}

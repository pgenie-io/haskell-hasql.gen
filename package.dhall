let Prelude = ./Prelude.dhall

let Sdk = ./Sdk.dhall

let Gen = Sdk.Gen

let Project = Gen.Project

let Config = ./Config/package.dhall

let Package = ./Package/package.dhall

let generate
    : Gen.Generate Config.Config
    = \(config : Config.Config) ->
      \(project : Project.Project) ->
        Package.compile project

in  Sdk.Gen.Gen Config.Config generate

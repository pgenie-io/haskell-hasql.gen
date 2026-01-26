let Sdk = ./Sdk.dhall

let CodegenKit = ./CodegenKit.dhall

let Config = ./Config.dhall

let Interpreters = ./Interpreters/package.dhall

in  Sdk.Gen
      Config.Type
      ( \(config : Config.Type) ->
        \(project : Sdk.Project.Project) ->
          let interpreterConfig =
                { rootNamespace =
                  [ CodegenKit.Name.toTextInPascal project.owner
                  , CodegenKit.Name.toTextInPascal project.name
                  ]
                }

          in  Interpreters.Project.run interpreterConfig project
      )

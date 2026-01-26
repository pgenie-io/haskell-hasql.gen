let Prelude = ./Prelude.dhall

let Sdk = ./Sdk.dhall

let CodegenKit = ./CodegenKit.dhall

let Config = ./Config/package.dhall

let Interpreter = ./Interpreter/package.dhall

in  Sdk.Gen
      Config.Config
      ( \(config : Config.Config) ->
        \(project : Sdk.Project.Project) ->
          let interpreterConfig =
                { rootNamespace =
                  [ CodegenKit.Name.toTextInPascal project.owner
                  , CodegenKit.Name.toTextInPascal project.name
                  ]
                }

          in  Interpreter.Modules.Project.run interpreterConfig project
      )

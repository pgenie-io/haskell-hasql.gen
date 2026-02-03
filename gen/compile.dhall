let Deps = ./Deps/package.dhall

let Sdk = Deps.Sdk

let CodegenKit = Deps.CodegenKit

let Config = ./Config.dhall

let ProjectInterpreter = ./Interpreters/Project.dhall

in  \(config : Optional Config) ->
    \(project : Sdk.Project.Project) ->
      let interpreterConfig =
            { rootNamespace =
              [ CodegenKit.Name.toTextInPascal project.space
              , CodegenKit.Name.toTextInPascal project.name
              ]
            }

      in  ProjectInterpreter.run interpreterConfig project

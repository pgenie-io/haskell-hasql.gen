let Deps = ./Deps/package.dhall

let Sdk = Deps.Sdk

let CodegenKit = Deps.CodegenKit

let Config = ./Config.dhall

let ProjectInterpreter = ./Interpreters/Project.dhall

in  \(config : Config) ->
    \(project : Sdk.Project.Project) ->
      let interpreterConfig =
            { rootNamespace =
              [ CodegenKit.Name.toTextInPascal project.owner
              , CodegenKit.Name.toTextInPascal project.name
              ]
            }

      in  ProjectInterpreter.run interpreterConfig project

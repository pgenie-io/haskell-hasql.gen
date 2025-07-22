let Prelude = ./../Prelude.dhall

let CodegenKit = ./../CodegenKit.dhall

let Sdk = ./../Sdk.dhall

let Modules = ../Modules/package.dhall

let Files = ../Files/package.dhall

let Algebras = ../Algebras/package.dhall

let Name = CodegenKit.Name

let Project = Sdk.Project

let Algebras = ../Algebras/package.dhall

let Module = Algebras.Module.Type

let Input = Sdk.Project.Project

let Output = List Sdk.Gen.File

let compile
    : Input -> Output
    = \(project : Input) ->
        let packageName = Name.toTextInKebab project.name

        let projectNamespace = Name.toTextInPascal project.name

        let statementModules =
              Prelude.List.map
                Sdk.Project.Query
                Modules.Statement.Output
                ( \(query : Sdk.Project.Query) ->
                    Modules.Statement.compile { projectNamespace, query }
                )
                project.queries

        let statementsModule =
              Modules.Statements.compile
                { projectNamespace
                , compiledStatementModules = statementModules
                }

        let files
            : List Sdk.Gen.File
            = let moduleFiles
                  : List Sdk.Gen.File
                  = let moduleFile =
                          \(x : Module) ->
                            { path = x.path, content = x.content }

                    let statementsFile = moduleFile statementsModule

                    let statementFiles =
                          Prelude.List.map
                            Module
                            Sdk.Gen.File
                            moduleFile
                            statementModules

                    let unprefixed = [ statementsFile ] # statementFiles

                    in  Prelude.List.map
                          Sdk.Gen.File
                          Sdk.Gen.File
                          ( \(x : Sdk.Gen.File) ->
                              x // { path = "library/${x.path}" }
                          )
                          unprefixed

              let cabalFile
                  : Sdk.Gen.File
                  = let publicModules = [ statementsModule.namespace ]

                    let privateModules =
                          Prelude.List.map
                            Module
                            Text
                            (\(x : Module) -> x.namespace)
                            statementModules

                    let version = "0"

                    let input =
                          { packageName
                          , version
                          , publicModules
                          , privateModules
                          }

                    in  Files.Cabal.compile input

              in  [ cabalFile ] # moduleFiles

        in  files

in  { Input, compile }

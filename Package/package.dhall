let Prelude = ../Prelude.dhall

let Lude = ../Lude.dhall

let CodegenKit = ../CodegenKit.dhall

let Sdk = ../Sdk.dhall

let Gen = Sdk.Gen

let Modules = ../Modules/package.dhall

let Files = ../Files/package.dhall

let Algebras = ../Algebras/package.dhall

let Name = CodegenKit.Name

let Project = Sdk.Project

let Module = Algebras.Module.Type

let Input = Sdk.Project.Project

let Output = Sdk.Gen.Result

let Result = Lude.Structures.Result

let compileStatementModules
    : Text ->
      List Sdk.Project.Query ->
        { warnings : List Gen.Warning
        , statementModules : List Modules.Statement.Output
        }
    = let State =
            { warnings : List Gen.Warning
            , statementModules : List Modules.Statement.Output
            }

      in  \(projectNamespace : Text) ->
          \(queries : List Sdk.Project.Query) ->
            Prelude.List.foldLeft
              Sdk.Project.Query
              queries
              State
              ( \(state : State) ->
                \(query : Sdk.Project.Query) ->
                  merge
                    { Failure =
                        \(dueTo : Gen.QueryError) ->
                              state
                          //  { warnings =
                                    [ Gen.Warning.QuerySkipped { query, dueTo }
                                    ]
                                  # state.warnings
                              }
                    , Success =
                        \(compiledStatement : Modules.Statement.Output) ->
                              state
                          //  { statementModules =
                                  [ compiledStatement ] # state.statementModules
                              }
                    }
                    (Modules.Statement.compile { projectNamespace, query })
              )
              { warnings = [] : List Gen.Warning
              , statementModules = [] : List Modules.Statement.Output
              }

let compile
    : Input -> Output
    = \(project : Input) ->
        let packageName = Name.toTextInKebab project.name

        let projectNamespace = Name.toTextInPascal project.name

        let compiledStatementModules =
              compileStatementModules projectNamespace project.queries

        let statementModules = compiledStatementModules.statementModules

        let statementsModule =
              Modules.Statements.compile
                { projectNamespace
                , compiledStatementModules = statementModules
                }

        let warnings = compiledStatementModules.warnings

        let files
            : List Gen.File
            = let moduleFiles
                  : List Gen.File
                  = let moduleFile =
                          \(x : Module) ->
                            { path = x.path, content = x.content }

                    let statementsFile = moduleFile statementsModule

                    let statementFiles =
                          Prelude.List.map
                            Module
                            Gen.File
                            moduleFile
                            statementModules

                    let unprefixed = [ statementsFile ] # statementFiles

                    in  Prelude.List.map
                          Gen.File
                          Gen.File
                          ( \(x : Gen.File) ->
                              x // { path = "library/${x.path}" }
                          )
                          unprefixed

              let cabalFile
                  : Gen.File
                  = let publicModules = [ statementsModule.namespace ]

                    let privateModules =
                          Prelude.List.map
                            Module
                            Text
                            (\(x : Module) -> x.namespace)
                            statementModules

                    let version =
                              "0."
                          ++  Prelude.Natural.show project.version.major
                          ++  "."
                          ++  Prelude.Natural.show project.version.minor
                          ++  "."
                          ++  Prelude.Natural.show project.version.patch

                    let input =
                          { packageName
                          , version
                          , publicModules
                          , privateModules
                          }

                    in  Files.Cabal.compile input

              in  [ cabalFile ] # moduleFiles

        in  { warnings, files }

in  { Input, compile }

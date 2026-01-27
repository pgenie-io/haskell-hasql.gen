let Deps = ../Deps/package.dhall

let Algebra = ./Algebra/package.dhall

let Sdk = Deps.Sdk

let Model = Deps.Sdk.Project

let Templates = ../Templates/package.dhall

let QueryGen = ./Query.dhall

let CustomTypeGen = ./CustomType.dhall

let Input = Model.Project

let Output = List Sdk.File.Type

let combineOutputs =
      \(config : Algebra.Config) ->
      \(input : Input) ->
      \(queries : List QueryGen.Output) ->
      \(customTypes : List CustomTypeGen.Output) ->
        let projectNamespace =
              Deps.Prelude.Text.concatSep "." config.rootNamespace

        let rootModuleFile
            : Sdk.File.Type
            = let path =
                    Templates.ModulePath.run
                      { namespace = config.rootNamespace }

              let content =
                    Templates.RootModule.run
                      { projectNamespace
                      , statementNames =
                          Deps.Prelude.List.map
                            QueryGen.Output
                            Text
                            ( \(query : QueryGen.Output) ->
                                query.statementModuleName
                            )
                            queries
                      }

              in  { path, content }

        let rootNamespace = Deps.Prelude.Text.concatSep "." config.rootNamespace

        let customTypeFiles
            : List Sdk.File.Type
            = Deps.Prelude.List.map
                CustomTypeGen.Output
                Sdk.File.Type
                ( \(customType : CustomTypeGen.Output) ->
                    { path = customType.modulePath
                    , content = customType.moduleContent
                    }
                )
                customTypes

        let statementFiles
            : List Sdk.File.Type
            = Deps.Prelude.List.map
                QueryGen.Output
                Sdk.File.Type
                ( \(query : QueryGen.Output) ->
                    { path = query.statementModulePath
                    , content = query.statementModuleContents
                    }
                )
                queries

        let cabalProjectFile =
              { path = "cabal.project"
              , content = Templates.CabalProjectFile.run {=}
              }

        let preludeFile =
              { path =
                  Templates.ModulePath.run
                    { namespace = config.rootNamespace # [ "Prelude" ] }
              , content = Templates.PreludeModule.run { projectNamespace }
              }

        let customTypesFile
            : Sdk.File.Type
            = { path =
                  Templates.ModulePath.run
                    { namespace = config.rootNamespace # [ "CustomTypes" ] }
              , content =
                  Templates.ReexportModule.run
                    { namespace = rootNamespace ++ ".CustomTypes"
                    , reexportedModules =
                        Deps.Prelude.List.map
                          CustomTypeGen.Output
                          Text
                          ( \(customType : CustomTypeGen.Output) ->
                              customType.moduleNamespace
                          )
                          customTypes
                    }
              }

        let statementsFile
            : Sdk.File.Type
            = { path =
                  Templates.ModulePath.run
                    { namespace = config.rootNamespace # [ "Statements" ] }
              , content =
                  Templates.ReexportModule.run
                    { namespace = rootNamespace ++ ".Statements"
                    , reexportedModules =
                        Deps.Prelude.List.map
                          QueryGen.Output
                          Text
                          ( \(query : QueryGen.Output) ->
                              query.statementModuleNamespace
                          )
                          queries
                    }
              }

        let cabalFile
            : Sdk.File.Type
            = let packageName =
                    Deps.CodegenKit.Name.concat input.owner [ input.name ]

              let packageName = Deps.CodegenKit.Name.toTextInKebab packageName

              let path = packageName ++ ".cabal"

              let content =
                    Templates.CabalFile.run
                      { packageName
                      , rootNamespace
                      , statementModuleNames =
                          Deps.Prelude.List.map
                            QueryGen.Output
                            Text
                            ( \(query : QueryGen.Output) ->
                                query.statementModuleName
                            )
                            queries
                      , customTypeNames =
                          Deps.Prelude.List.map
                            CustomTypeGen.Output
                            Text
                            ( \(customType : CustomTypeGen.Output) ->
                                customType.moduleName
                            )
                            customTypes
                      , version = "0"
                      }

              in  { path, content }

        in      [ cabalProjectFile
                , cabalFile
                , preludeFile
                , rootModuleFile
                , customTypesFile
                , statementsFile
                ]
              # customTypeFiles
              # statementFiles
            : List Sdk.File.Type

let run =
      \(config : Algebra.Config) ->
      \(input : Input) ->
        let compiledQueries
            : Sdk.Compiled.Type (List (Optional QueryGen.Output))
            = Sdk.Compiled.traverseList
                Deps.Sdk.Project.Query
                (Optional QueryGen.Output)
                ( \(query : Deps.Sdk.Project.Query) ->
                    Deps.Typeclasses.Classes.Alternative.optional
                      Sdk.Compiled.Type
                      Sdk.Compiled.alternative
                      QueryGen.Output
                      (QueryGen.run config query)
                )
                input.queries

        let compiledQueries
            : Sdk.Compiled.Type (List QueryGen.Output)
            = Sdk.Compiled.map
                (List (Optional QueryGen.Output))
                (List QueryGen.Output)
                (Deps.Prelude.List.unpackOptionals QueryGen.Output)
                compiledQueries

        let compiledCustomTypes
            : Sdk.Compiled.Type (List CustomTypeGen.Output)
            = Sdk.Compiled.traverseList
                Deps.Sdk.Project.CustomType
                CustomTypeGen.Output
                (CustomTypeGen.run config)
                input.customTypes

        let files
            : Sdk.Compiled.Type (List Sdk.File.Type)
            = Sdk.Compiled.map2
                (List QueryGen.Output)
                (List CustomTypeGen.Output)
                (List Sdk.File.Type)
                (combineOutputs config input)
                compiledQueries
                compiledCustomTypes

        in  files

in  Algebra.module Input Output run

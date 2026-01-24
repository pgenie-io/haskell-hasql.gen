let Algebra = ../Algebra.dhall

let Sdk = Algebra.Sdk

let Lude = Algebra.Lude

let Model = Algebra.Model

let Templates = ../../Templates/package.dhall

let QueryGen = ./Query.dhall

let Input = Model.Project

let Output = List Sdk.File.Type

let combineOutputs =
      \(input : Input) ->
      \(compiledQueries : List QueryGen.Output) ->
        let rootNamespaceAsList =
              [ Algebra.Name.toTextInPascal input.owner
              , Algebra.Name.toTextInPascal input.name
              ]

        let rootNamespace =
              Algebra.Prelude.Text.concatSep "." rootNamespaceAsList

        let Query =
              { statementModuleName : Text
              , statementModuleNamespace : Text
              , statementModulePath : Text
              , statementModuleContents : Text
              }

        let compiledQueries =
              Algebra.Prelude.List.map
                QueryGen.Output
                Query
                (\(query : QueryGen.Output) -> query rootNamespaceAsList)
                compiledQueries

        let rootModuleFile
            : Sdk.File.Type
            = let path =
                    Templates.ModulePath.run { namespace = rootNamespaceAsList }

              let content =
                    Templates.RootModule.run
                      { projectNamespace =
                          Algebra.Prelude.Text.concatSep "." rootNamespaceAsList
                      , statementNames =
                          Algebra.Prelude.List.map
                            Query
                            Text
                            (\(query : Query) -> query.statementModuleName)
                            compiledQueries
                      }

              in  { path, content }

        let statementFiles
            : List Sdk.File.Type
            = Algebra.Prelude.List.map
                Query
                Sdk.File.Type
                ( \(query : Query) ->
                    { path = query.statementModulePath
                    , content = query.statementModuleContents
                    }
                )
                compiledQueries

        let cabalFile
            : Sdk.Gen.File
            = let packageName = Algebra.Name.concat input.owner [ input.name ]

              let packageName = Algebra.Name.toTextInKebab packageName

              let path = packageName ++ ".cabal"

              let content =
                    Templates.CabalFile.run
                      { packageName
                      , rootNamespace =
                          Algebra.Prelude.Text.concatSep "." rootNamespaceAsList
                      , statementModuleNames =
                          Algebra.Prelude.List.map
                            Query
                            Text
                            (\(query : Query) -> query.statementModuleName)
                            compiledQueries
                      , declaredTypeNames = [] : List Text
                      , version = "0"
                      }

              in  { path, content }

        in  [ cabalFile, rootModuleFile ] # statementFiles : List Sdk.File.Type

let run
    : Input -> Sdk.Compiled.Type Output
    = \(input : Input) ->
        let compiledQueries
            : Sdk.Compiled.Type (List (Optional QueryGen.Output))
            = Sdk.Compiled.traverseList
                Algebra.Model.Query
                (Optional QueryGen.Output)
                ( \(query : Algebra.Model.Query) ->
                    Algebra.Typeclasses.Classes.Alternative.optional
                      Sdk.Compiled.Type
                      Sdk.Compiled.alternative
                      QueryGen.Output
                      (QueryGen.run query)
                )
                input.queries

        let compiledQueries
            : Sdk.Compiled.Type (List QueryGen.Output)
            = Sdk.Compiled.map
                (List (Optional QueryGen.Output))
                (List QueryGen.Output)
                (Algebra.Prelude.List.unpackOptionals QueryGen.Output)
                compiledQueries

        let files
            : Sdk.Compiled.Type (List Sdk.File.Type)
            = Sdk.Compiled.map
                (List QueryGen.Output)
                (List Sdk.File.Type)
                (combineOutputs input)
                compiledQueries

        in  files

in  Algebra.module Input Output run

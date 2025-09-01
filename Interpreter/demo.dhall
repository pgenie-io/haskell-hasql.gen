let Algebra = ./Algebra.dhall

let Modules = ./Modules/package.dhall

let fixture1 = Algebra.Sdk.Fixtures._1

let compiledQueries
    : Algebra.Sdk.Compiled.Type (List (Optional Modules.Query.Output))
    = Algebra.Sdk.Compiled.traverseList
        Algebra.Model.Query
        (Optional Modules.Query.Output)
        ( \(query : Algebra.Model.Query) ->
            Algebra.Lude.Algebras.Alternative.optional
              Algebra.Sdk.Compiled.Type
              Algebra.Sdk.Compiled.alternative
              Modules.Query.Output
              (Modules.Query.run query)
        )
        fixture1.queries

let compiledQueries
    : Algebra.Sdk.Compiled.Type (List Modules.Query.Output)
    = Algebra.Sdk.Compiled.map
        (List (Optional Modules.Query.Output))
        (List Modules.Query.Output)
        (Algebra.Prelude.List.unpackOptionals Modules.Query.Output)
        compiledQueries

let reports =
      let renderReport =
            \(report : { path : List Text, message : Text }) ->
              ''
              - Warning: ${report.message}
                Context: ${Algebra.Prelude.Text.concatMap
                             Text
                             (\(segment : Text) -> "\n" ++ "    - " ++ segment)
                             report.path}
              ''

      in  ''
          ${Algebra.Prelude.Text.concatMapSep
              "\n"
              { path : List Text, message : Text }
              renderReport
              compiledQueries.reports}
          ''

let files =
      let renderQuery
          : Modules.Query.Output -> Text
          = \(query : Modules.Query.Output) ->
              let query = query [ "PGenie", "Demo" ]

              in  Algebra.Prelude.Text.concat
                    [ query.statementModulePath
                    , ":"
                    , "\n"
                    , Algebra.Lude.Extensions.Text.indent
                        2
                        ("\n" ++ query.statementModuleContents)
                    ]

      let renderQueries
          : List Modules.Query.Output -> Text
          = Algebra.Prelude.Text.concatMapSep
              "\n\n"
              Modules.Query.Output
              renderQuery

      in  Algebra.Prelude.Optional.fold
            (List Modules.Query.Output)
            compiledQueries.result
            Text
            renderQueries
            ""

in  ''
    ${reports}

    ${files}
    ''

-- | The API module that reexports and organizes everything to fit the UX.
let Algebra = ./Algebra/package.dhall

let Deps = ../Deps/package.dhall

let Params = { projectNamespace : Text, statementNames : List Text }

in  Algebra.module
      Params
      ( \(params : Params) ->
          ''
          module ${params.projectNamespace}
            ( -- * Execution
              
              -- ** Levels
              Hasql.Pipeline,
              Hasql.Session,
              Hasql.Transaction,
              
              -- ** Execution functions
              runInPipeline,
              runInSession,
              runInTransaction,
              
              -- * Statement Typeclass
              IsStatement,
              
              -- * Statements
              ${Deps.Lude.Extensions.Text.indent
                  4
                  ( Deps.Prelude.Text.concatMapSep
                      ''
                      ,
                      ''
                      Text
                      ( \(statementName : Text) ->
                          ''
                          -- ** ${statementName} statement
                          module ${params.projectNamespace}.Statements.${statementName}''
                      )
                      params.statementNames
                  )}
            )
          where

          ${Deps.Prelude.Text.concatMapSep
              "\n"
              Text
              ( \(statementName : Text) ->
                  "import ${params.projectNamespace}.Statements.${statementName}"
              )
              params.statementNames}

          runInPipeline :: (IsStatement a) => a -> Hasql.Pipeline b

          ''
      )

let Algebra = ../Algebras/Template/package.dhall

let Prelude = ../Prelude.dhall

let Lude = ../Lude.dhall

let Member = { fieldName : Text, sig : Text }

let Params =
      { queryName : Text
      , sqlForDocs : Text
      , srcPath : Text
      , typeName : Text
      , fields : List Text
      }

in  Algebra.module
      Params
      ( \(params : Params) ->
          ''
          -- |
          -- Parameters for the @${params.queryName}@ query.
          --
          -- == SQL Template
          --
          -- > ${Lude.Extensions.Text.prefixEachLine "-- > " params.sqlForDocs}
          --
          -- == Source Path
          --
          -- > ${params.srcPath}
          --
          data ${params.typeName} = ${params.typeName}
            { ${Lude.Extensions.Text.indent
                  4
                  ( Prelude.Text.concatSep
                      ''
                      ,
                      ''
                      params.fields
                  )}
            }
            deriving stock (Eq, Show)
          ''
      )

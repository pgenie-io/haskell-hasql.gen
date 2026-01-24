let Algebra = ../Algebras/Template/package.dhall

let Prelude = ../Prelude.dhall

let Lude = ../Lude.dhall

let Member = { fieldName : Text, sig : Text }

let Params =
      { queryName : Text
      , sqlForDocs : Text
      , srcPath : Text
      , typeName : Text
      , members : List Member
      }

in  Algebra.module
      Params
      ( \(params : Params) ->
          let fieldDecls =
                Prelude.Text.concatMapSep
                  ''
                  ,
                  ''
                  Member
                  ( \(member : Member) ->
                      member.fieldName ++ " :: " ++ member.sig
                  )
                  params.members

          in  ''
              -- |
              -- Parameters for the @${params.queryName}@ query.
              --
              -- == SQL Template
              --
              -- > ${Lude.Extensions.Text.prefixEachLine
                       "-- > "
                       params.sqlForDocs}
              --
              -- == Source Path
              --
              -- > ${params.srcPath}
              --
              data ${params.typeName} = ${params.typeName}
                { ${Lude.Extensions.Text.indent 4 fieldDecls}
                }
                deriving stock (Eq, Show)
              ''
      )

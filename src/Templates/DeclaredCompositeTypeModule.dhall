let Algebra = ../Algebras/Template/package.dhall

let Prelude = ../Prelude.dhall

let Lude = ../Lude.dhall

let Field = { name : Text, sig : Text }

let Params =
      { moduleName : Text
      , typeName : Text
      , pgTypeName : Text
      , fields : List Field
      }

in  Algebra.module
      Params
      ( \(params : Params) ->
          ''
          module ${params.moduleName} where

          -- |
          -- Representation of the @${params.pgTypeName}@ user-declared PostgreSQL record type.
          data ${params.typeName} = ${params.typeName}
            { ${Lude.Extensions.Text.indent
                  4
                  ( Prelude.Text.concatMapSep
                      ''
                      ,
                      ''
                      Field
                      (\(field : Field) -> field.name ++ " :: " ++ field.sig)
                      params.fields
                  )}
            }
            deriving stock (Eq, Show)
          ''
      )

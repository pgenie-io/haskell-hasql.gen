let Algebra = ../Algebras/Template/package.dhall

let Prelude = ../Prelude.dhall

let Lude = ../Lude.dhall

let Params = { name : Text, fields : List Text }

in  Algebra.module
      Params
      ( \(params : Params) ->
          ''
          data ${params.name} = ${params.name}
            { ${Lude.Extensions.Text.indent
                  4
                  ( Prelude.Text.concatSep
                      ''
                      ,
                      ''
                      params.fields
                  )}
            }''
      )

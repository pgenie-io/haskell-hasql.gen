let Algebra = ./Algebra/package.dhall

let Deps = ../Deps/package.dhall

let Params = { name : Text, fields : List Text }

in  Algebra.module
      Params
      ( \(params : Params) ->
          ''
          data ${params.name} = ${params.name}
            { ${Deps.Lude.Extensions.Text.indent
                  4
                  ( Deps.Prelude.Text.concatSep
                      ''
                      ,
                      ''
                      params.fields
                  )}
            }''
      )

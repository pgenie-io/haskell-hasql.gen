let Algebra = ../Algebras/Template/package.dhall

let Prelude = ../Prelude.dhall

let Lude = ../Lude.dhall

let Params = { namespace : Text, reexportedModules : List Text }

in  Algebra.module
      Params
      ( \(params : Params) ->
          let importsBlock =
                Prelude.Text.concatMapSep
                  "\n"
                  Text
                  (\(namespace : Text) -> "import ${namespace}")
                  params.reexportedModules

          let exportsBlock =
                Prelude.Text.concatMapSep
                  "\n"
                  Text
                  (\(statement : Text) -> "module ${statement},")
                  params.reexportedModules

          in  ''
              module ${params.namespace} 
                ( ${Lude.Extensions.Text.indent 4 exportsBlock}
                )
              where

              ${importsBlock}
              ''
      )

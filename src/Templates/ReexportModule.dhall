let Algebra = ./Algebra/package.dhall

let Deps = ../Deps/package.dhall

let Params = { namespace : Text, reexportedModules : List Text }

in  Algebra.module
      Params
      ( \(params : Params) ->
          let importsBlock =
                Deps.Prelude.Text.concatMapSep
                  "\n"
                  Text
                  (\(namespace : Text) -> "import ${namespace}")
                  params.reexportedModules

          let exportsBlock =
                Deps.Prelude.Text.concatMapSep
                  "\n"
                  Text
                  (\(statement : Text) -> "module ${statement},")
                  params.reexportedModules

          in  ''
              module ${params.namespace} 
                ( ${Deps.Lude.Extensions.Text.indent 4 exportsBlock}
                )
              where

              ${importsBlock}
              ''
      )

let Algebra = ./Algebra/package.dhall

let Deps = ../Deps/package.dhall

let Haddock = ./Haddock.dhall

let Params =
      { haddock : Optional Text
      , namespace : Text
      , reexportedModules : List Text
      }

in  Algebra.module
      Params
      ( \(params : Params) ->
          let haddock = Haddock.run params.haddock

          let importsBlock =
                Deps.Prelude.Text.concatMapSep
                  "\n"
                  Text
                  (\(module : Text) -> "import ${module}")
                  params.reexportedModules

          let exportsBlock =
                Deps.Prelude.Text.concatMapSep
                  "\n"
                  Text
                  (\(module : Text) -> "module ${module},")
                  params.reexportedModules

          in  ''
              ${haddock}module ${params.namespace} 
                ( ${Deps.Lude.Extensions.Text.indent 4 exportsBlock}
                )
              where

              ${importsBlock}
              ''
      )

let Algebra = ./Algebra/package.dhall

let Deps = ../Deps/package.dhall

let Params =
      { haddock : Optional Text
      , namespace : Text
      , reexportedModules : List Text
      }

in  Algebra.module
      Params
      ( \(params : Params) ->
          let haddock =
                merge
                  { None = ""
                  , Some =
                      \(unprefixedText : Text) ->
                            "-- | "
                        ++  Deps.Lude.Extensions.Text.prefixEachLine
                              "-- "
                              unprefixedText
                        ++  "\n"
                  }
                  params.haddock

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

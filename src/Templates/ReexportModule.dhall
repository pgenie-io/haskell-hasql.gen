let Algebra = ./Algebra/package.dhall

let Deps = ../Deps/package.dhall

let Haddock = ./Haddock.dhall

let ReexportedModule = { haddock : Optional Text, namespace : Text }

let Params =
      { haddock : Optional Text
      , namespace : Text
      , reexportedModules : List ReexportedModule
      }

in      Algebra.module
          Params
          ( \(params : Params) ->
              let haddock = Haddock.run { isDoc = True, text = params.haddock }

              let importsBlock =
                    Deps.Prelude.Text.concatMapSep
                      "\n"
                      ReexportedModule
                      ( \(module : ReexportedModule) ->
                          "import ${module.namespace}"
                      )
                      params.reexportedModules

              let exportsBlock =
                    Deps.Prelude.Text.concatMapSep
                      "\n"
                      ReexportedModule
                      ( \(module : ReexportedModule) ->
                          let haddock =
                                Haddock.run
                                  { isDoc = False, text = module.haddock }

                          in  "${haddock}module ${module.namespace},"
                      )
                      params.reexportedModules

              in  ''
                  ${haddock}module ${params.namespace} 
                    ( ${Deps.Lude.Extensions.Text.indent 4 exportsBlock}
                    )
                  where

                  ${importsBlock}
                  ''
          )
    //  { ReexportedModule }

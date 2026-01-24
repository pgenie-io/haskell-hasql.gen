let Prelude = ../../Prelude.dhall

let Lude = ../../Lude.dhall

let Code = ./Type.dhall

let Env = ./Env.dhall

let Result = ./Result.dhall

let concat
    : List Code -> Code
    = \(list : List Code) ->
      \(env : Env) ->
        Prelude.List.foldLeft
          Code
          list
          Result
          ( \(state : Result) ->
            \(elementCode : Code) ->
              let env
                  : Env
                  = env // { registeredImports = state.registeredImports }

              let elementResult
                  : Result
                  = elementCode env

              let registeredImports = elementResult.registeredImports

              let rendering = state.rendering ++ elementResult.rendering

              in  { registeredImports, rendering }
          )
          { registeredImports = env.registeredImports, rendering = "" }

in  concat

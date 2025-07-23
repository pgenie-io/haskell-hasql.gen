let Code = ./Type.dhall

let Env = ./Env.dhall

let Result = ./Result.dhall

in  \(rendering : Text) ->
        ( \(env : Env) ->
            { registeredImports = env.registeredImports, rendering } : Result
        )
      : Code

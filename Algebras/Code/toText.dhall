let Prelude = ../../Prelude.dhall

let Lude = ../../Lude.dhall

let Namespace = ../Namespace/package.dhall

let Code = ./Type.dhall

let Env = ./Env.dhall

let Result = ./Result.dhall

let toText
    : Code -> Text
    = \(code : Code) ->
        let result =
              code
                { importAliases = Prelude.Map.empty Namespace.Type Text
                , registeredImports = [] : List Namespace.Type
                }

        in  result.rendering

in  toText

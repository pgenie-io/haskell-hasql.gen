let Prelude = ../../Prelude.dhall

let Lude = ../../Lude.dhall

let Code = ./Type.dhall

let Namespace = ../Namespace/package.dhall

let Env = ./Env.dhall

let Result = ./Result.dhall

let fromText = ./fromText.dhall

let importing
    : Namespace.Type -> (Text -> Code) -> Code
    = \(namespace : Namespace.Type) ->
      \(continuation : Text -> Code) ->
      \(env : Env) ->
        let registeredImports =
              Lude.Extensions.List.insertIntoDeduped
                Namespace.Type
                Namespace.order
                env.registeredImports
                namespace

        let aliasLookup
            : Optional Text
            = Lude.Extensions.Map.lookup
                Namespace.Type
                Namespace.order
                Text
                env.importAliases
                namespace

        let reference
            : Text
            = merge
                { None = Namespace.toText namespace
                , Some = \(alias : Text) -> alias
                }
                aliasLookup

        let importAliases = env.importAliases

        in  continuation reference { registeredImports, importAliases } : Result

in  importing

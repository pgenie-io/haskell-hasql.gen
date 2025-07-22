let Prelude = ../../Prelude.dhall

let Lude = ../../Lude.dhall

let Self = ./Type.dhall

let Namespace = ../Namespace/package.dhall

let importing
    : Namespace.Type -> (Text -> Self) -> Self
    = \(namespace : Namespace.Type) ->
      \(continuation : Text -> Self) ->
      \ ( env
        : { importAliases : Prelude.Map.Type Namespace.Type Text
          , registeredImports : List Namespace.Type
          }
        ) ->
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

        let reference =
              merge
                { None = Namespace.toText namespace
                , Some = \(alias : Text) -> alias
                }
                aliasLookup

        let importAliases = env.importAliases

        in  continuation reference { registeredImports, importAliases }

in  importing

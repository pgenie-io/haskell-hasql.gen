let Prelude = ../../Prelude.dhall

let Lude = ../../Lude.dhall

let Code = ./Type.dhall

let Namespace = ../Namespace/package.dhall

let Env = ./Env.dhall

let Result = ./Result.dhall

let fromText = ./fromText.dhall

let renderQualifiedImport
    : Prelude.Map.Type Namespace.Type Text -> Namespace.Type -> Text
    = \(importAliases : Prelude.Map.Type Namespace.Type Text) ->
      \(namespace : Namespace.Type) ->
        let aliasLookup
            : Optional Text
            = Lude.Extensions.Map.lookup
                Namespace.Type
                Namespace.order
                Text
                importAliases
                namespace

        in  merge
              { None = "import qualified ${Namespace.toText namespace}"
              , Some =
                  \(alias : Text) ->
                    "import qualified ${Namespace.toText namespace} as ${alias}"
              }
              aliasLookup

let render
    : Prelude.Map.Type Namespace.Type Text -> Code -> Text
    = \(importAliases : Prelude.Map.Type Namespace.Type Text) ->
      \(code : Code) ->
        let registeredImports = [] : List Namespace.Type

        let body = code { registeredImports, importAliases }

        let imports =
              Prelude.Text.concatMapSep
                "\n"
                Namespace.Type
                (renderQualifiedImport importAliases)
                body.registeredImports

        in  ''
            ${imports}

            ${body.rendering}
            ''

in  render

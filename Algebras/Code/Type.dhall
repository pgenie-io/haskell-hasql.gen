let Prelude = ../../Prelude.dhall

let Namespace = ../Namespace/package.dhall

in  { importAliases : Prelude.Map.Type Namespace.Type Text
    , registeredImports : List Namespace.Type
    } ->
      { registeredImports : List Namespace.Type, rendering : Text }

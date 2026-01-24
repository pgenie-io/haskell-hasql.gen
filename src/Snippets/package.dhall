-- Code snippets. Common constructs. Patterns.
let recordDataDecl
    : { name : Text, fields : List { name : Text, sig : Text } } -> Text
    = ./recordDataDecl.dhall

let reexportModule
    : { namespace : Text, reexportedModules : List Text } -> Text
    = ./reexportModule.dhall

let stringLiteral
    : Text -> Text
    = ./stringLiteral.dhall

in  { recordDataDecl, reexportModule, stringLiteral }

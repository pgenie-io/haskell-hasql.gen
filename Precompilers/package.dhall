let CodegenKit = ../CodegenKit.dhall

let Name = CodegenKit.Name

let Sdk = ../Sdk.dhall

let Model = Sdk.Project

let queryFragments
    : List Model.QueryFragment ->
        { stringLiteral : Text, names : List Name.Type }
    = ./queryFragments.dhall

in  { queryFragments }

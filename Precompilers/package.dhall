-- Collection of precompilers structured after the model components.
let CodegenKit = ../CodegenKit.dhall

let Name = CodegenKit.Name

let Sdk = ../Sdk.dhall

let Model = Sdk.Project

let queryFragments
    : List Model.QueryFragment ->
        { stringLiteral : Text, names : List Name.Type }
    = ./queryFragments.dhall

let primitive
    : Model.Primitive ->
        { sig : Optional Text
        , encoder : Optional Text
        , decoder : Optional Text
        }
    = ./primitive.dhall

let scalar
    : Model.Scalar ->
        { sig : Optional Text
        , encoder : Optional Text
        , decoder : Optional Text
        }
    = ./scalar.dhall

in  { queryFragments, primitive, scalar }

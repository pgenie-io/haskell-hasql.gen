-- Collection of precompilers structured after the model components.
let CodegenKit = ../CodegenKit.dhall

let Lude = ../Lude.dhall

let Name = CodegenKit.Name

let Sdk = ../Sdk.dhall

let Model = Sdk.Project

let Result = Lude.Structures.Result

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
        { sig : Result.Type Text Text
        , encoder : Result.Type Text Text
        , decoder : Result.Type Text Text
        }
    = ./scalar.dhall

let dimensional = ./dimensional.dhall

in  { queryFragments, primitive, scalar, dimensional }

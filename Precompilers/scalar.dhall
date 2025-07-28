let Prelude = ../Prelude.dhall

let Lude = ../Lude.dhall

let Sdk = ../Sdk.dhall

let Model = Sdk.Project

let Snippets = ../Snippets/package.dhall

let CodegenKit = ../CodegenKit.dhall

let Name = CodegenKit.Name

let Result = Lude.Structures.Result

let primitive = ./primitive.dhall

in  \(model : Model.Scalar) ->
      merge
        { Primitive = primitive
        , Custom =
            \(name : Name.Type) ->
              let name = Name.toTextInPascal name

              let sig = "CustomTypes.${name}"

              in  { encoder = Some "Mapping.valueEncoder @${sig}"
                  , decoder = Some "Mapping.valueDecoder @${sig}"
                  , sig = Some sig
                  }
        }
        model
      : { sig : Optional Text
        , encoder : Optional Text
        , decoder : Optional Text
        }

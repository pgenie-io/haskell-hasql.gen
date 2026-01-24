let Prelude = ../Prelude.dhall

let Lude = ../Lude.dhall

let Sdk = ../Sdk.dhall

let Model = Sdk.Project

let Snippets = ../Snippets/package.dhall

let CodegenKit = ../CodegenKit.dhall

let Name = CodegenKit.Name

let Result = Lude.Structures.Result

let primitive = ./primitive.dhall

let Rendering = Result.Type Text Text

in  \(model : Model.Scalar) ->
      merge
        { Primitive =
            \(model : Model.Primitive) ->
              let liftPrimitiveRendering =
                    \(rendering : Optional Text) ->
                      merge
                        { None =
                            Rendering.Failure
                              "Unsupported type: ${Model.Primitive/toText
                                                     model}"
                        , Some = Rendering.Success
                        }
                        rendering

              let primitive = primitive model

              in  { encoder = liftPrimitiveRendering primitive.encoder
                  , decoder = liftPrimitiveRendering primitive.decoder
                  , sig = liftPrimitiveRendering primitive.sig
                  }
        , Custom =
            \(name : Name.Type) ->
              let name = Name.toTextInPascal name

              let sig = "CustomTypes.${name}"

              in  { encoder = Rendering.Success "Mapping.valueEncoder @${sig}"
                  , decoder = Rendering.Success "Mapping.valueDecoder @${sig}"
                  , sig = Rendering.Success sig
                  }
        }
        model
      : { sig : Rendering, encoder : Rendering, decoder : Rendering }

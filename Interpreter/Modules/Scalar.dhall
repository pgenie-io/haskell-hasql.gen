let Algebra = ../Algebra.dhall

let Sdk = Algebra.Sdk

let Lude = Algebra.Lude

let Model = Algebra.Model

let Primitive = ./Primitive.dhall

let Input = Model.Scalar

let Output = { sig : Text, decoderExp : Text }

let Result = Sdk.Compiled.Type Output

let run =
      \(input : Input) ->
        merge
          { Primitive =
              \(primitive : Model.Primitive) ->
                Sdk.Compiled.map
                  Primitive.Output
                  Output
                  ( \(primitive : Primitive.Output) ->
                      { sig = primitive.sig
                      , decoderExp = "Decoders.${primitive.decoderName}"
                      }
                  )
                  (Primitive.run primitive)
          , Custom =
              \(name : Model.Name) ->
                let nameText = Algebra.Name.toTextInCamel name

                in  Sdk.Compiled.ok
                      Output
                      { sig = "CustomTypes.${Algebra.Name.toTextInPascal name}"
                      , decoderExp =
                          "CustomTypes.${Algebra.Name.toTextInCamel name}"
                      }
          }
          input

in  Algebra.module Input Output run

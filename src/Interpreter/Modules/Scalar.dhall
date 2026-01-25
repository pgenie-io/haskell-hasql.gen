let Algebra = ../Algebra.dhall

let Sdk = Algebra.Sdk

let Lude = Algebra.Lude

let Model = Algebra.Model

let Primitive = ./Primitive.dhall

let Input = Model.Scalar

let Output = { sig : Text, encoderExp : Text, decoderExp : Text }

let run =
      \(config : Algebra.Config) ->
      \(input : Input) ->
        merge
          { Primitive =
              \(primitive : Model.Primitive) ->
                Sdk.Compiled.map
                  Primitive.Output
                  Output
                  ( \(primitive : Primitive.Output) ->
                      { sig = primitive.sig
                      , encoderExp = "Encoders.${primitive.codecName}"
                      , decoderExp = "Decoders.${primitive.codecName}"
                      }
                  )
                  (Primitive.run config primitive)
          , Custom =
              \(name : Model.Name) ->
                let nameText = Algebra.Name.toTextInCamel name

                in  Sdk.Compiled.ok
                      Output
                      { sig =
                          "DeclaredTypes.${Algebra.Name.toTextInPascal name}"
                      , encoderExp =
                          "IsScalar.encoderOf @${Algebra.Name.toTextInCamel
                                                   name}"
                      , decoderExp =
                          "IsScalar.decoderOf @${Algebra.Name.toTextInCamel
                                                   name}"
                      }
          }
          input

in  Algebra.module Input Output run

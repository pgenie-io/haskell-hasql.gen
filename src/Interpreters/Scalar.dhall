let Algebra = ./Algebra/package.dhall

let Sdk = Algebra.Sdk

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
                Sdk.Compiled.ok
                  Output
                  { sig = "CustomTypes.${Algebra.Name.toTextInPascal name}"
                  , encoderExp =
                      "IsScalar.scalarEncoder @${Algebra.Name.toTextInCamel name}"
                  , decoderExp =
                      "IsScalar.scalarDecoder @${Algebra.Name.toTextInCamel name}"
                  }
          }
          input

in  Algebra.module Input Output run

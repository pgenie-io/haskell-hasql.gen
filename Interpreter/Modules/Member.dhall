let Algebra = ../Algebra.dhall

let Sdk = Algebra.Sdk

let Lude = Algebra.Lude

let Model = Algebra.Model

let Value = ./Value.dhall

let Input = Model.Member

let Output = { fieldName : Text, sig : Text, decoderExp : Text }

let Result = Algebra.Result Output

let run =
      \(input : Input) ->
        Algebra.Result/flatMap
          Value.Output
          Output
          ( \(value : Value.Output) ->
              let fieldName = Algebra.Name.toTextInCamel input.name

              in  if    input.isNullable
                  then  Result.Success
                          { fieldName
                          , sig = "Maybe ${value.sig}"
                          , decoderExp = "Decoders.nullable ${value.decoderExp}"
                          }
                  else  Result.Success
                          { fieldName
                          , sig = value.sig
                          , decoderExp =
                              "Decoders.nonNullable ${value.decoderExp}"
                          }
          )
          ( Algebra.Result/nest
              Value.Output
              input.rawName
              (Value.run input.value)
          )

in  Algebra.module Input Output run

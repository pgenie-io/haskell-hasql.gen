let Algebra = ../Algebra.dhall

let Sdk = Algebra.Sdk

let Lude = Algebra.Lude

let Model = Algebra.Model

let Value = ./Value.dhall

let Input = Model.Member

let Output = { fieldName : Text, sig : Text, decoderExp : Text }

let Error = { name : Text, error : Value.Error }

let Result = Lude.Structures.Result.Type Error Output

let run =
      \(input : Input) ->
        merge
          { Failure =
              \(error : Value.Error) ->
                Result.Failure { name = input.rawName, error }
          , Success =
              \(value : Value.Output) ->
                let fieldName = Algebra.Name.toTextInCamel input.name

                in  if    input.isNullable
                    then  Result.Success
                            { fieldName
                            , sig = "Maybe ${value.sig}"
                            , decoderExp =
                                "Decoders.nullable ${value.decoderExp}"
                            }
                    else  Result.Success
                            { fieldName
                            , sig = value.sig
                            , decoderExp =
                                "Decoders.nonNullable ${value.decoderExp}"
                            }
          }
          (Value.run input.value)

in  Algebra.module Input Output Error run

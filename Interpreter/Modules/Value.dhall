let Algebra = ../Algebra.dhall

let Sdk = Algebra.Sdk

let Lude = Algebra.Lude

let Model = Algebra.Model

let Scalar = ./Scalar.dhall

let Input = Model.Value

let Output = { sig : Text, decoderExp : Text }

let Result = Algebra.Result Output

let run =
      \(input : Input) ->
        Algebra.Result/flatMap
          Scalar.Output
          Output
          ( \(scalar : Scalar.Output) ->
              Algebra.Prelude.Optional.fold
                Model.ArraySettings
                input.arraySettings
                Result
                ( \(arraySettings : Model.ArraySettings) ->
                    Result.Failure (Algebra.Error/message "TODO")
                )
                ( Result.Success
                    { sig = scalar.sig, decoderExp = scalar.decoderExp }
                )
          )
          (Scalar.run input.scalar)

in  Algebra.module Input Output run

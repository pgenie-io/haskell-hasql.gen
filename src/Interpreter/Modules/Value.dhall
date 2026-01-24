let Algebra = ../Algebra.dhall

let Sdk = Algebra.Sdk

let Lude = Algebra.Lude

let Model = Algebra.Model

let Scalar = ./Scalar.dhall

let Input = Model.Value

let Output = { sig : Text, decoderExp : Text }

let Result = Sdk.Compiled.Type Output

let run =
      \(input : Input) ->
        Sdk.Compiled.flatMap
          Scalar.Output
          Output
          ( \(scalar : Scalar.Output) ->
              Algebra.Prelude.Optional.fold
                Model.ArraySettings
                input.arraySettings
                Result
                ( \(arraySettings : Model.ArraySettings) ->
                    Sdk.Compiled.message Output "TODO"
                )
                ( Sdk.Compiled.ok
                    Output
                    { sig = scalar.sig, decoderExp = scalar.decoderExp }
                )
          )
          (Scalar.run input.scalar)

in  Algebra.module Input Output run

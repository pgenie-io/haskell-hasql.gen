let Algebra = ../Algebra.dhall

let Sdk = Algebra.Sdk

let Lude = Algebra.Lude

let Model = Algebra.Model

let Scalar = ./Scalar.dhall

let Input = Model.Value

let Output = { sig : Text, decoderExp : Text }

let Error = < Scalar : Scalar.Error | Todo >

let Result = Lude.Structures.Result.Type Error Output

let run =
      \(input : Input) ->
        merge
          { Failure =
              \(error : Scalar.Error) -> Result.Failure (Error.Scalar error)
          , Success =
              \(scalar : Scalar.Output) ->
                Algebra.Prelude.Optional.fold
                  Model.ArraySettings
                  input.arraySettings
                  Result
                  ( \(arraySettings : Model.ArraySettings) ->
                      Result.Failure Error.Todo
                  )
                  ( Result.Success
                      { sig = scalar.sig, decoderExp = scalar.decoderExp }
                  )
          }
          (Scalar.run input.scalar)

in  Algebra.module Input Output Error run

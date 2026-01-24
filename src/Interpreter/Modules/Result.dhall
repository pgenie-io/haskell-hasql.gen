let Algebra = ../Algebra.dhall

let ResultRows = ./ResultRows.dhall

let Input = Algebra.Model.Result

let Output = Text -> { typeDecls : List Text, decoderExp : Text }

let Result = Algebra.Sdk.Compiled.Type Output

let run
    : Input -> Result
    = \(input : Input) ->
        Algebra.Prelude.Optional.fold
          ResultRows.Input
          input
          Result
          ( \(resultRows : ResultRows.Input) ->
              Algebra.Sdk.Compiled.map
                ResultRows.Output
                Output
                ( \(resultRows : ResultRows.Output) ->
                  \(typeNameBase : Text) ->
                    let resultRows = resultRows typeNameBase

                    in  { typeDecls =
                          [ resultRows.resultTypeDecl, resultRows.rowTypeDecl ]
                        , decoderExp = resultRows.decoderExp
                        }
                )
                (ResultRows.run resultRows)
          )
          (Algebra.Sdk.Compiled.message Output "TODO: Result")

in  Algebra.module Input Output run

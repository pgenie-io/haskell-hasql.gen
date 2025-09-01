let Algebra = ../Algebra.dhall

let ResultRows = ./ResultRows.dhall

let Input = Algebra.Model.Result

let Output = Text -> { typeDecls : List Text, decoderExp : Text }

let Result = Algebra.Result Output

let run
    : Input -> Result
    = \(input : Input) ->
        Algebra.Prelude.Optional.fold
          ResultRows.Input
          input
          Result
          ( \(resultRows : ResultRows.Input) ->
              Algebra.Result/map
                ResultRows.Output
                Output
                ( \(resultRows : ResultRows.Output) ->
                  \(typeNameBase : Text) ->
                    let resultRows = resultRows typeNameBase

                    in  { typeDecls =
                          [ resultRows.resultTypeDecl, resultRows.rowTypeDecl ]
                        , decoderExp = ""
                        }
                )
                (ResultRows.run resultRows)
          )
          (Result.Failure (Algebra.Error/message "TODO"))

in  Algebra.module Input Output run

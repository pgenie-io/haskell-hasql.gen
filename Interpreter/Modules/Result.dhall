let Algebra = ../Algebra.dhall

let ResultRows = ./ResultRows.dhall

let Input = Algebra.Model.Result

let Output = Text -> { typeDecls : List Text, decoderExp : Text }

let Error = < ResultRows : ResultRows.Error | Todo >

let Result = Algebra.Lude.Structures.Result.Type Error Output

let run
    : Input -> Result
    = \(input : Input) ->
        Algebra.Prelude.Optional.fold
          ResultRows.Input
          input
          Result
          ( \(resultRows : ResultRows.Input) ->
              merge
                { Success =
                    \(resultRows : ResultRows.Output) ->
                      Result.Success
                        ( \(typeNameBase : Text) ->
                            let resultRows = resultRows typeNameBase

                            in  { typeDecls =
                                  [ resultRows.resultTypeDecl
                                  , resultRows.rowTypeDecl
                                  ]
                                , decoderExp = ""
                                }
                        )
                , Failure =
                    \(error : ResultRows.Error) ->
                      Result.Failure (Error.ResultRows error)
                }
                (ResultRows.run resultRows)
          )
          (Result.Failure Error.Todo)

in  Algebra.module Input Output Error run

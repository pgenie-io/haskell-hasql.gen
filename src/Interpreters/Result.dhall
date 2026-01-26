let Deps = ../Deps/package.dhall

let Algebra = ./Algebra/package.dhall

let ResultRows = ./ResultRows.dhall

let Input = Deps.Sdk.Project.Result

let Output = Text -> { typeDecls : List Text, decoderExp : Text }

let Result = Deps.Sdk.Compiled.Type Output

let run =
      \(config : Algebra.Config) ->
      \(input : Input) ->
        Deps.Prelude.Optional.fold
          ResultRows.Input
          input
          Result
          ( \(resultRows : ResultRows.Input) ->
              Deps.Sdk.Compiled.map
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
                (ResultRows.run config resultRows)
          )
          (Deps.Sdk.Compiled.message Output "TODO: Result")

in  Algebra.module Input Output run

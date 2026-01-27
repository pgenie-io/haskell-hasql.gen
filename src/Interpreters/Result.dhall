let Deps = ../Deps/package.dhall

let Algebra = ./Algebra/package.dhall

let ResultRows = ./ResultRows.dhall

let Input = Deps.Sdk.Project.Result

let Output = Text -> { typeDecls : Text, decoderExp : Text }

let Result = Deps.Sdk.Compiled.Type Output

let run =
      \(config : Algebra.Config) ->
      \(input : Input) ->
        Deps.Prelude.Optional.fold
          ResultRows.Input
          input
          Result
          (ResultRows.run config)
          ( Deps.Sdk.Compiled.ok
              Output
              ( \(typeNameBase : Text) ->
                  { typeDecls =
                      ''
                      type ${typeNameBase}Result = Int
                      ''
                  , decoderExp = "fromIntegral <\$> Decoders.rowsAffected"
                  }
              )
          )

in  Algebra.module Input Output run

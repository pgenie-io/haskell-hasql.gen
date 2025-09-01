let Algebra = ../Algebra.dhall

let ResultRows = ./ResultRows.dhall

let Input = Algebra.Model.Query

let Output =
      { statementModuleName : Text
      , statementModulePath : Text
      , statementModuleContents : Text
      }

let Result = Algebra.Result Output

let run
    : Input -> Result
    = \(input : Input) -> Result.Failure (Algebra.Error/message "TODO")

in  Algebra.module Input Output run

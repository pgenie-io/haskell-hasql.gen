let Algebra = ../Algebra.dhall

let ResultRows = ./ResultRows.dhall

let Input = Algebra.Model.Query

let Output =
      { statementModuleName : Text
      , statementModulePath : Text
      , statementModuleContents : Text
      }

let Error = < Column : ResultRows.Error | Todo >

let Result = Algebra.Lude.Structures.Result.Type Error Output

let run
    : Input -> Result
    = \(input : Input) -> Result.Failure Error.Todo

in  Algebra.module Input Output Error run

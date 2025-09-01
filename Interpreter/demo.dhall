let Algebra = ./Algebra.dhall

let Modules = ./Modules/package.dhall

let fixture1 = Algebra.Sdk.Fixtures._1

in  Algebra.Lude.Structures.Result.traverseList
      Modules.Result.Error
      Algebra.Model.Query
      Modules.Result.Output
      (\(query : Algebra.Model.Query) -> Modules.Result.run query.result)
      fixture1.queries

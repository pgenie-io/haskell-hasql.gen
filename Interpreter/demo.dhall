let Algebra = ./Algebra.dhall

let Modules = ./Modules/package.dhall

let fixture1 = Algebra.Sdk.Fixtures._1

in  Algebra.Result/traverseList
      Algebra.Model.Query
      Modules.Result.Output
      (\(query : Algebra.Model.Query) -> Modules.Result.run query.result)
      fixture1.queries

let Algebra = ./Algebra.dhall

let Modules = ./Modules/package.dhall

let fixture1 = Algebra.Sdk.Fixtures._1

in  Algebra.Prelude.List.map
      Algebra.Model.Query
      Modules.Query.Result
      (\(query : Algebra.Model.Query) -> Modules.Query.run query)
      fixture1.queries

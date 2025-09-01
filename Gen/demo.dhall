let Algebra = ./Algebra.dhall

let ProjectGen = ./Modules/Project.dhall

let fixture1 = Algebra.Sdk.Fixtures._1

in  Algebra.Sdk.Compiled.toPlainText
      ( Algebra.Sdk.Compiled.map
          ProjectGen.Output
          Text
          ( \(files : List Algebra.Sdk.File.Type) ->
              Algebra.Prelude.Text.concatMapSep
                "\n\n"
                Algebra.Sdk.File.Type
                Algebra.Sdk.File.toPlainText
                files
          )
          (ProjectGen.run fixture1)
      )

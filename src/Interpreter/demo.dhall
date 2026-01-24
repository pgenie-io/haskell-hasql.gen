let Algebra = ./Algebra.dhall

let Modules = ./Modules/package.dhall

let ProjectModule = Modules.Project

let fixture1 = Algebra.Sdk.Fixtures._1

in  Algebra.Sdk.Compiled.toPlainText
      ( Algebra.Sdk.Compiled.map
          ProjectModule.Output
          Text
          ( \(files : List Algebra.Sdk.File.Type) ->
              Algebra.Prelude.Text.concatMapSep
                "\n\n"
                Algebra.Sdk.File.Type
                Algebra.Sdk.File.toPlainText
                files
          )
          (ProjectModule.run fixture1)
      )

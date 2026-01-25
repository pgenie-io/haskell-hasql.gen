let Algebra = ./Algebra.dhall

let Modules = ./Modules/package.dhall

let ProjectModule = Modules.Project

let fixture = Algebra.Sdk.Fixtures._1

let config
    : Algebra.Config
    = { rootNamespace =
        [ Algebra.Name.toTextInPascal fixture.owner
        , Algebra.Name.toTextInPascal fixture.name
        ]
      }

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
          (ProjectModule.run config fixture)
      )

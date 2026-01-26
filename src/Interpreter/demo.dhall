let Algebra = ./Algebra.dhall

let Modules = ./Modules/package.dhall

let Sdk = ../Sdk.dhall

let ProjectGen = Modules.Project

let project = Sdk.Fixtures._1

let config
    : Algebra.Config
    = { rootNamespace =
        [ Algebra.Name.toTextInPascal project.owner
        , Algebra.Name.toTextInPascal project.name
        ]
      }

let compiledFiles
    : Sdk.Compiled.Type (List Sdk.File.Type)
    = ProjectGen.run config project

in  Sdk.Compiled.toPlainText
      ( Sdk.Compiled.map
          ProjectGen.Output
          Text
          ( \(files : List Sdk.File.Type) ->
              Algebra.Prelude.Text.concatMapSep
                "\n\n"
                Sdk.File.Type
                Sdk.File.toPlainText
                files
          )
          (ProjectGen.run config project)
      )

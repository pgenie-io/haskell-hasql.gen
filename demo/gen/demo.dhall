let Deps = ../../src/Deps/package.dhall

let Prelude = Deps.Prelude

let Sdk = Deps.Sdk

let Config = ../../src/Config.dhall

let Gen = ../../src/package.dhall

let project = Sdk.Fixtures._1

let config = Config.default

let compiledFiles
    : Sdk.Compiled.Type (List Sdk.File.Type)
    = Gen.generate config project

let reports
    : Text
    = Sdk.Compiled.toReportsText (List Sdk.File.Type) compiledFiles

let files
    : Prelude.Map.Type Text Text
    = merge
        { None = [ Prelude.Map.keyText "gen-errors.yaml" reports ]
        , Some =
            \(files : List Sdk.File.Type) ->
                [ Prelude.Map.keyText "gen-warnings.yaml" reports ]
              # Prelude.List.map
                  Sdk.File.Type
                  (Prelude.Map.Entry Text Text)
                  ( \(file : Sdk.File.Type) ->
                      Prelude.Map.keyText file.path file.content
                  )
                  files
        }
        compiledFiles.result

in  files

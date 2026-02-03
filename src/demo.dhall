-- Intended to be executed with:
--
-- ```bash
-- dhall to-directory-tree --file src/demo.dhall --output demo-output --allow-path-separators
-- ```
--
-- It however assumes that you have a proper version of Dhall installed.
--
-- The changes required for this to work are in [this PR](https://github.com/dhall-lang/dhall-haskell/pull/2448).
-- You can acquire this version of Dhall by installing from https://github.com/nikita-volkov/dhall-haskell.
let Deps = ./Deps/package.dhall

let Prelude = Deps.Prelude

let Sdk = Deps.Sdk

let project = Sdk.Fixtures._1

let config = {=}

let compiledFiles
    : Sdk.Compiled.Type (List Sdk.File.Type)
    = ./compile.dhall config project

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
                  Sdk.File.toMapEntry
                  files
        }
        compiledFiles.result

in  files

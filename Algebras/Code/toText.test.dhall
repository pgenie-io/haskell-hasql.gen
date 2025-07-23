let Prelude = ../../Prelude.dhall

let toText = ./toText.dhall

let fromText = ./fromText.dhall

let concat = ./concat.dhall

in  { toTextFunction =
      { simpleCode = assert : toText (fromText "simple text") === "simple text"
      , emptyCode = assert : toText (fromText "") === ""
      , codeWithoutImports =
            assert
          :     toText (fromText "data User = User { name :: String }")
            ===  "data User = User { name :: String }"
      }
    , toTextWithConcatenation =
      { multipleCodeBlocks =
            assert
          :     toText
                  ( concat
                      [ fromText "module ", fromText "Main", fromText " where" ]
                  )
            ===  "module Main where"
      , emptyList = assert : toText (concat ([] : List ./Type.dhall)) === ""
      , singleItem = assert : toText (concat [ fromText "single" ]) === "single"
      }
    }

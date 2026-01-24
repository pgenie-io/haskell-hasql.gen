let Prelude = ../../Prelude.dhall

let fromText = ./fromText.dhall

let toText = ./toText.dhall

in  { fromTextFunction =
      { simpleText = assert : toText (fromText "hello world") === "hello world"
      , emptyText = assert : toText (fromText "") === ""
      , multilineText =
            assert
          :     toText
                  ( fromText
                      ''
                      line1
                      line2
                      line3''
                  )
            ===  ''
                 line1
                 line2
                 line3''
      , textWithSpecialCharacters =
            assert
          :     toText (fromText "text with \"quotes\" and 'apostrophes'")
            ===  "text with \"quotes\" and 'apostrophes'"
      , haskellCode =
            assert
          :     toText
                  ( fromText
                      ''
                      module Main where
                      main = putStrLn "Hello"''
                  )
            ===  ''
                 module Main where
                 main = putStrLn "Hello"''
      }
    }

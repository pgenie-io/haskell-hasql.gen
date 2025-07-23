let Prelude = ../../Prelude.dhall

let concat = ./concat.dhall

let fromText = ./fromText.dhall

let toText = ./toText.dhall

in  { concatFunction =
      { emptyList = assert : toText (concat ([] : List ./Type.dhall)) === ""
      , singleElement =
          assert : toText (concat [ fromText "hello" ]) === "hello"
      , multipleElements =
            assert
          :     toText
                  (concat [ fromText "hello", fromText " ", fromText "world" ])
            ===  "hello world"
      , manyElements =
            assert
          :     toText
                  ( concat
                      [ fromText "module "
                      , fromText "Main "
                      , fromText
                          ''
                          where
                          ''
                      , fromText "main "
                      , fromText "= "
                      , fromText "putStrLn "
                      , fromText "\"Hello\""
                      ]
                  )
            ===  ''
                 module Main where
                 main = putStrLn "Hello"''
      }
    , concatenationOrder =
      { preservesOrder =
            assert
          :     toText
                  ( concat
                      [ fromText "first", fromText "second", fromText "third" ]
                  )
            ===  "firstsecondthird"
      , differentOrder =
            assert
          :     toText (concat [ fromText "A", fromText "B", fromText "C" ])
            ===  "ABC"
      }
    }

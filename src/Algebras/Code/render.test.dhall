let Prelude = ../../Prelude.dhall

let Code = ./Type.dhall

let Lude = ../../Lude.dhall

let CodegenKit = ../../CodegenKit.dhall

let importing = ./importing.dhall

let render = ./render.dhall

let fromText = ./fromText.dhall

let Namespace = ../Namespace/package.dhall

let Name = CodegenKit.Name

let LatinChars = Lude.Structures.LatinChars

let LatinChar = LatinChars.Char.Type

let dataListNamespace =
      Namespace.separate
        (Namespace.word LatinChar.D [ LatinChar.A, LatinChar.T, LatinChar.A ])
        [ Namespace.word LatinChar.L [ LatinChar.I, LatinChar.S, LatinChar.T ] ]

let controlMonadNamespace =
      Namespace.separate
        ( Namespace.word
            LatinChar.C
            [ LatinChar.O
            , LatinChar.N
            , LatinChar.T
            , LatinChar.R
            , LatinChar.O
            , LatinChar.L
            ]
        )
        [ Namespace.word
            LatinChar.M
            [ LatinChar.O, LatinChar.N, LatinChar.A, LatinChar.D ]
        ]

let emptyAliasMap = [] : Prelude.Map.Type Namespace.Type Text

let singleAliasMap =
        [ { mapKey = dataListNamespace, mapValue = "L" } ]
      : Prelude.Map.Type Namespace.Type Text

let multipleAliasMap =
        [ { mapKey = dataListNamespace, mapValue = "L" }
        , { mapKey = controlMonadNamespace, mapValue = "CM" }
        ]
      : Prelude.Map.Type Namespace.Type Text

in  { empty =
        let emptyCode = fromText ""

        in  assert : render emptyAliasMap emptyCode === "\n\n\n"
    , oneImport =
        let code
            : Code
            = importing
                dataListNamespace
                ( \(dataListRef : Text) ->
                    fromText "${dataListRef}.length myList"
                )

        in  { unaliased =
                  assert
                :     render emptyAliasMap code
                  ===  ''
                       import qualified Data.List

                       Data.List.length myList
                       ''
            , aliased =
                  assert
                :     render singleAliasMap code
                  ===  ''
                       import qualified Data.List as L

                       L.length myList
                       ''
            }
    , noImports =
        let code
            : Code
            = fromText "main = putStrLn \"Hello, World!\""

        in    assert
            :     render emptyAliasMap code
              ===  ''


                   main = putStrLn "Hello, World!"
                   ''
    , multipleImports =
        -- Code that registers multiple imports
        let code
            : Code
            = importing
                dataListNamespace
                ( \(dataListRef : Text) ->
                    importing
                      controlMonadNamespace
                      ( \(controlMonadRef : Text) ->
                          fromText
                            "${controlMonadRef}.forM_ myList (print . ${dataListRef}.length)"
                      )
                )

        in    assert
            :     render multipleAliasMap code
              ===  ''
                   import qualified Control.Monad as CM
                   import qualified Data.List as L

                   CM.forM_ myList (print . L.length)
                   ''
    }

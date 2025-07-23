let Prelude = ../../Prelude.dhall

let CodegenKit = ../../CodegenKit.dhall

let toText = ./toText.dhall

let Self = ./Type.dhall

let Name = CodegenKit.Name

let Lude = ../../Lude.dhall

let LatinChars = Lude.Structures.LatinChars

let LatinChar = LatinChars.Char.Type

let separate = ./separate.dhall

let squash = ./squash.dhall

let singleton = ./singleton.dhall

let word = ./word.dhall

let singleWordName =
      \(head : LatinChars.Char.Type) ->
      \(tail : List LatinChars.Char.Type) ->
        Name.fromLatinChars (LatinChars.fromHeadAndTail head tail)

let singleNamespace =
      word
        LatinChar.P
        [ LatinChar.R
        , LatinChar.E
        , LatinChar.L
        , LatinChar.U
        , LatinChar.D
        , LatinChar.E
        ]

let twoLevelNamespace =
      separate
        (word LatinChar.D [ LatinChar.A, LatinChar.T, LatinChar.A ])
        [ word LatinChar.L [ LatinChar.I, LatinChar.S, LatinChar.T ] ]

let threeLevelNamespace =
      separate
        ( word
            LatinChar.C
            [ LatinChar.O
            , LatinChar.N
            , LatinChar.T
            , LatinChar.R
            , LatinChar.O
            , LatinChar.L
            ]
        )
        [ word
            LatinChar.M
            [ LatinChar.O, LatinChar.N, LatinChar.A, LatinChar.D ]
        , word
            LatinChar.R
            [ LatinChar.E, LatinChar.A, LatinChar.D, LatinChar.E, LatinChar.R ]
        ]

in  { singleElement = assert : toText singleNamespace === "Prelude"
    , twoElements = assert : toText twoLevelNamespace === "Data.List"
    , threeElements =
        assert : toText threeLevelNamespace === "Control.Monad.Reader"
    , nameFormatting.basicNames
      =
        let namespace =
              separate
                ( squash
                    (word LatinChar.M [ LatinChar.Y ])
                    [ word
                        LatinChar.M
                        [ LatinChar.O
                        , LatinChar.D
                        , LatinChar.U
                        , LatinChar.L
                        , LatinChar.E
                        ]
                    ]
                )
                [ word
                    LatinChar.S
                    [ LatinChar.U
                    , LatinChar.B
                    , LatinChar.M
                    , LatinChar.O
                    , LatinChar.D
                    , LatinChar.U
                    , LatinChar.L
                    , LatinChar.E
                    ]
                ]

        in  assert : toText namespace === "MyModule.Submodule"
    , edgeCases.singleCharacterNames
      =
        let namespace =
              separate
                (word LatinChar.A ([] : List LatinChars.Char.Type))
                [ word LatinChar.B ([] : List LatinChars.Char.Type)
                , word LatinChar.C ([] : List LatinChars.Char.Type)
                ]

        in  assert : toText namespace === "A.B.C"
    }

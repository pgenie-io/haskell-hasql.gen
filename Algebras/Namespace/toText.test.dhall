let Prelude = ../../Prelude.dhall

let CodegenKit = ../../CodegenKit.dhall

let toText = ./toText.dhall

let NamespaceType = ./Type.dhall

let Name = CodegenKit.Name

let Lude = ../../Lude.dhall

let LatinChars = Lude.Structures.LatinChars

let LatinChar = LatinChars.Char.Type

let singleWordName =
      \(head : LatinChars.Char.Type) ->
      \(tail : List LatinChars.Char.Type) ->
        Name.fromLatinChars (LatinChars.fromHeadAndTail head tail)

let singleNamespace =
        { head =
            singleWordName
              LatinChar.P
              [ LatinChar.R
              , LatinChar.E
              , LatinChar.L
              , LatinChar.U
              , LatinChar.D
              , LatinChar.E
              ]
        , tail = [] : List Name.Type
        }
      : NamespaceType

let twoLevelNamespace =
        { head =
            singleWordName LatinChar.D [ LatinChar.A, LatinChar.T, LatinChar.A ]
        , tail =
          [ singleWordName LatinChar.L [ LatinChar.I, LatinChar.S, LatinChar.T ]
          ]
        }
      : NamespaceType

let threeLevelNamespace =
        { head =
            singleWordName
              LatinChar.C
              [ LatinChar.O
              , LatinChar.N
              , LatinChar.T
              , LatinChar.R
              , LatinChar.O
              , LatinChar.L
              ]
        , tail =
          [ singleWordName
              LatinChar.M
              [ LatinChar.O, LatinChar.N, LatinChar.A, LatinChar.D ]
          , singleWordName
              LatinChar.R
              [ LatinChar.E
              , LatinChar.A
              , LatinChar.D
              , LatinChar.E
              , LatinChar.R
              ]
          ]
        }
      : NamespaceType

in  { toTextFunction =
      { singleElement = assert : toText singleNamespace === "Prelude"
      , twoElements = assert : toText twoLevelNamespace === "Data.List"
      , threeElements =
          assert : toText threeLevelNamespace === "Control.Monad.Reader"
      , nameFormatting.basicNames
        =
          let namespace =
                  { head =
                      singleWordName
                        LatinChar.M
                        [ LatinChar.Y
                        , LatinChar.M
                        , LatinChar.O
                        , LatinChar.D
                        , LatinChar.U
                        , LatinChar.L
                        , LatinChar.E
                        ]
                  , tail =
                    [ singleWordName
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
                  }
                : NamespaceType

          in  assert : toText namespace === "Mymodule.Submodule"
      , edgeCases.singleCharacterNames
        =
          let namespace =
                  { head = singleWordName LatinChar.A ([] : List LatinChar)
                  , tail =
                    [ singleWordName LatinChar.B ([] : List LatinChar)
                    , singleWordName LatinChar.C ([] : List LatinChar)
                    ]
                  }
                : NamespaceType

          in  assert : toText namespace === "A.B.C"
      }
    }

let Prelude = ../../Prelude.dhall

let Lude = ../../Lude.dhall

let CodegenKit = ../../CodegenKit.dhall

let squash = ./squash.dhall

let NamespaceType = ./Type.dhall

let Name = CodegenKit.Name

let LatinChars = Lude.Structures.LatinChars

let LatinChar = LatinChars.Char.Type

let singleton = ./singleton.dhall

let word = ./word.dhall

let toText = ./toText.dhall

let dataNamespace = word LatinChar.D [ LatinChar.A, LatinChar.T, LatinChar.A ]

let listNamespace = word LatinChar.L [ LatinChar.I, LatinChar.S, LatinChar.T ]

let mapNamespace = word LatinChar.M [ LatinChar.A, LatinChar.P ]

let controlNamespace =
      word
        LatinChar.C
        [ LatinChar.O
        , LatinChar.N
        , LatinChar.T
        , LatinChar.R
        , LatinChar.O
        , LatinChar.L
        ]

let monadNamespace =
      word LatinChar.M [ LatinChar.O, LatinChar.N, LatinChar.A, LatinChar.D ]

let readerNamespace =
      word
        LatinChar.R
        [ LatinChar.E, LatinChar.A, LatinChar.D, LatinChar.E, LatinChar.R ]

in  { twoNamespaces.basicTest
      =
        let result = squash dataNamespace [ listNamespace ]

        in  assert : toText result === "DataList"
    , threeNamespaces.basicTest
      =
        let result = squash controlNamespace [ monadNamespace, readerNamespace ]

        in  assert : toText result === "ControlMonadReader"
    , singleNamespaceWithEmpty.basicTest
      =
        let result = squash dataNamespace ([] : List NamespaceType)

        in  assert : toText result === "Data"
    , multipleNamespaces.fourNamespaces
      =
        let utilsNamespace =
              word
                LatinChar.U
                [ LatinChar.T, LatinChar.I, LatinChar.L, LatinChar.S ]

        let helpersNamespace =
              word
                LatinChar.H
                [ LatinChar.E
                , LatinChar.L
                , LatinChar.P
                , LatinChar.E
                , LatinChar.R
                , LatinChar.S
                ]

        let result =
              squash
                dataNamespace
                [ listNamespace, utilsNamespace, helpersNamespace ]

        in  assert : toText result === "DataListUtilsHelpers"
    , singleCharacterNames.basicTest
      =
        let aNamespace = word LatinChar.A ([] : List LatinChar)

        let bNamespace = word LatinChar.B ([] : List LatinChar)

        let cNamespace = word LatinChar.C ([] : List LatinChar)

        let result = squash aNamespace [ bNamespace, cNamespace ]

        in  assert : toText result === "ABC"
    }

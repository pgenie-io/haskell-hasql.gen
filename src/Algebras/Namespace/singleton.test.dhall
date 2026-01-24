let Prelude = ../../Prelude.dhall

let Lude = ../../Lude.dhall

let CodegenKit = ../../CodegenKit.dhall

let singleton = ./singleton.dhall

let NamespaceType = ./Type.dhall

let Name = CodegenKit.Name

let LatinChars = Lude.Structures.LatinChars

let LatinChar = LatinChars.Char.Type

let toText = ./toText.dhall

let simpleTestName =
      Name.fromLatinChars
        ( LatinChars.fromHeadAndTail
            LatinChar.T
            [ LatinChar.E, LatinChar.S, LatinChar.T ]
        )

let moduleTestName =
      Name.fromLatinChars
        ( LatinChars.fromHeadAndTail
            LatinChar.M
            [ LatinChar.O, LatinChar.D, LatinChar.U, LatinChar.L, LatinChar.E ]
        )

let preludeTestName =
      Name.fromLatinChars
        ( LatinChars.fromHeadAndTail
            LatinChar.P
            [ LatinChar.R
            , LatinChar.E
            , LatinChar.L
            , LatinChar.U
            , LatinChar.D
            , LatinChar.E
            ]
        )

in  { basicTest =
        let result = singleton simpleTestName

        in  assert : toText result === "Test"
    , moduleTest =
        let result = singleton moduleTestName

        in  assert : toText result === "Module"
    , preludeTest =
        let result = singleton preludeTestName

        in  assert : toText result === "Prelude"
    , singleCharacterName =
        let singleCharName =
              Name.fromLatinChars
                (LatinChars.fromHeadAndTail LatinChar.A ([] : List LatinChar))

        let result = singleton singleCharName

        in  assert : toText result === "A"
    }

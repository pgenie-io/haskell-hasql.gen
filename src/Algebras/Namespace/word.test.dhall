let Prelude = ../../Prelude.dhall

let Lude = ../../Lude.dhall

let CodegenKit = ../../CodegenKit.dhall

let word = ./word.dhall

let NamespaceType = ./Type.dhall

let Name = CodegenKit.Name

let LatinChars = Lude.Structures.LatinChars

let LatinChar = LatinChars.Char.Type

let toText = ./toText.dhall

in  { basicWords =
      { dataWord =
          let result =
                word LatinChar.D [ LatinChar.A, LatinChar.T, LatinChar.A ]

          in  assert : toText result === "Data"
      , listWord =
          let result =
                word LatinChar.L [ LatinChar.I, LatinChar.S, LatinChar.T ]

          in  assert : toText result === "List"
      , controlWord =
          let result =
                word
                  LatinChar.C
                  [ LatinChar.O
                  , LatinChar.N
                  , LatinChar.T
                  , LatinChar.R
                  , LatinChar.O
                  , LatinChar.L
                  ]

          in  assert : toText result === "Control"
      }
    , edgeCases =
      { singleCharacter =
          let result = word LatinChar.A ([] : List LatinChar)

          in  assert : toText result === "A"
      , longerWord =
          let result =
                word
                  LatinChar.P
                  [ LatinChar.R
                  , LatinChar.E
                  , LatinChar.L
                  , LatinChar.U
                  , LatinChar.D
                  , LatinChar.E
                  ]

          in  assert : toText result === "Prelude"
      , moduleWord =
          let result =
                word
                  LatinChar.M
                  [ LatinChar.O
                  , LatinChar.D
                  , LatinChar.U
                  , LatinChar.L
                  , LatinChar.E
                  ]

          in  assert : toText result === "Module"
      }
    , alphabeticalTests =
      { letterZ =
          let result =
                word
                  LatinChar.Z
                  [ LatinChar.E, LatinChar.B, LatinChar.R, LatinChar.A ]

          in  assert : toText result === "Zebra"
      , letterB =
          let result = word LatinChar.B ([] : List LatinChar)

          in  assert : toText result === "B"
      }
    }

let Prelude = ../../Prelude.dhall

let Code = ./Type.dhall

let Lude = ../../Lude.dhall

let CodegenKit = ../../CodegenKit.dhall

let importing = ./importing.dhall

let concat = ./concat.dhall

let fromText = ./fromText.dhall

let toText = ./toText.dhall

let Namespace = ../Namespace/package.dhall

let Name = CodegenKit.Name

let LatinChars = Lude.Structures.LatinChars

let LatinChar = LatinChars.Char.Type

let singleWordName =
      \(head : LatinChars.Char.Type) ->
      \(tail : List LatinChars.Char.Type) ->
        Name.fromLatinChars (LatinChars.fromHeadAndTail head tail)

let namespace =
      \(head : Name.Type) ->
      \(tail : List Name.Type) ->
        { head, tail } : Namespace.Type

let testNamespace1 =
        { head =
            singleWordName LatinChar.D [ LatinChar.A, LatinChar.T, LatinChar.A ]
        , tail =
          [ singleWordName LatinChar.L [ LatinChar.I, LatinChar.S, LatinChar.T ]
          ]
        }
      : Namespace.Type

let testNamespace2 =
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
          ]
        }
      : Namespace.Type

in  { importingFunction =
      { simpleImport =
          let result =
                importing
                  testNamespace1
                  ( \(reference : Code) ->
                      concat [ reference, fromText " :: [a] -> Int" ]
                  )

          in  assert : toText result === "Data.List :: [a] -> Int"
      , importWithContinuation =
          let result =
                importing testNamespace2 (\(reference : Code) -> reference)

          in  assert : toText result === "Control.Monad"
      , importWithComplexContinuation =
          let result =
                importing
                  testNamespace1
                  ( \(reference : Code) ->
                      concat [ fromText "length ", reference ]
                  )

          in  assert : toText result === "length Data.List"
      }
    }

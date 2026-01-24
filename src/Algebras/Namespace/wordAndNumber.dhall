let Prelude = ../../Prelude.dhall

let CodegenKit = ../../CodegenKit.dhall

let Name = CodegenKit.Name

let Lude = ../../Lude.dhall

let LatinChars = Lude.Structures.LatinChars

let LatinChar = Lude.Structures.LatinChar

let Self = ./Type.dhall

let singleton = ./singleton.dhall

in  \(head : LatinChar.Type) ->
    \(tail : List LatinChar.Type) ->
    \(number : Natural) ->
      let name =
            Name.fromLatinCharsAndNumber
              (LatinChars.fromHeadAndTail head tail)
              number

      in  singleton name

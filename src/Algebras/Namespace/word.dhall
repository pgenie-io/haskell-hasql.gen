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
      singleton (Name.fromLatinChars (LatinChars.fromHeadAndTail head tail))

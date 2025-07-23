-- Merges namespaces into a single namespace by separating.
let Prelude = ../../Prelude.dhall

let CodegenKit = ../../CodegenKit.dhall

let Name = CodegenKit.Name

let Self = ./Type.dhall

in  \(head : Self) ->
    \(tail : List Self) ->
      { head = head.head
      , tail =
            head.tail
          # Prelude.List.concatMap
              Self
              Name.Type
              (Prelude.NonEmpty.toList Name.Type)
              tail
      }

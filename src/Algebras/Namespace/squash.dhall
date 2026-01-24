-- Merges namespaces into one with a single concatenated name.
let Prelude = ../../Prelude.dhall

let CodegenKit = ../../CodegenKit.dhall

let Name = CodegenKit.Name

let Self = ./Type.dhall

in  \(head : Self) ->
    \(tail : List Self) ->
      Prelude.NonEmpty.singleton
        Name.Type
        ( Name.concat
            head.head
            (   head.tail
              # Prelude.List.concatMap
                  Self
                  Name.Type
                  (Prelude.NonEmpty.toList Name.Type)
                  tail
            )
        )

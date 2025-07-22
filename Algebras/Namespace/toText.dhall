let Prelude = ../../Prelude.dhall

let Self = ./Type.dhall

let CodegenKit = ../../CodegenKit.dhall

in  \(namespace : Self) ->
      let head = CodegenKit.Name.toTextInPascal namespace.head

      let tail =
            Prelude.Text.concatMap
              CodegenKit.Name.Type
              ( \(name : CodegenKit.Name.Type) ->
                  "." ++ CodegenKit.Name.toTextInPascal name
              )
              namespace.tail

      in  head ++ tail

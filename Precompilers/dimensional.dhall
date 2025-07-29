let Prelude = ../Prelude.dhall

let Lude = ../Lude.dhall

let Sdk = ../Sdk.dhall

let Model = Sdk.Project

let Snippets = ../Snippets/package.dhall

let CodegenKit = ../CodegenKit.dhall

let Name = CodegenKit.Name

let Result = Lude.Structures.Result

let Rendering = Result.Type Text Text

let scalar = ./scalar.dhall

let applyDimensionalityToSig
    : Natural -> Text -> Text
    = \(dimensionality : Natural) ->
        let prefix = Prelude.Text.replicate dimensionality "Vector ("

        let suffix = Prelude.Text.replicate dimensionality ")"

        in  \(sig : Text) -> "${prefix}${sig}${suffix}"

in  \(model : Model.Dimensional) ->
      let scalar = scalar model.scalar

      let sig
          : Rendering
          = Result.mapSuccess
              Text
              Text
              Text
              (applyDimensionalityToSig model.dimensionality)
              scalar.sig

      in  { sig }

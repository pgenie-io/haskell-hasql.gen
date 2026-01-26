let Algebra = ../Algebras/Template/package.dhall

let Prelude = ../Prelude.dhall

let Lude = ../Lude.dhall

let DimensionalityDecoderExp = ./DimensionalityDecoderExp.dhall

let Params =
      { name : Text
      , nullable : Bool
      , dimensionality : Natural
      , elementIsNullable : Bool
      }

in  Algebra.module
      Params
      ( \(params : Params) ->
              "(."
          ++  params.name
          ++  ") <\$> "
          ++  ( if    params.nullable
                then  "Decoders.nullable"
                else  "Decoders.nonNullable"
              )
          ++  " ("
          ++  DimensionalityDecoderExp.run
                { dimensionality = params.dimensionality
                , elementIsNullable = params.elementIsNullable
                , elementExp = "valueDecoder"
                }
          ++  ")"
      )

let Algebra = ./Algebra/package.dhall

let DimensionalityDecoderExp = ./DimensionalityDecoderExp.dhall

let Params =
      { nullable : Bool, dimensionality : Natural, elementIsNullable : Bool }

in  Algebra.module
      Params
      ( \(params : Params) ->
              ( if    params.nullable
                then  "Decoders.nullable"
                else  "Decoders.nonNullable"
              )
          ++  " ("
          ++  DimensionalityDecoderExp.run
                { dimensionality = params.dimensionality
                , elementIsNullable = params.elementIsNullable
                , elementExp = "Mapping.scalarDecoder"
                }
          ++  ")"
      )

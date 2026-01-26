let Algebra = ./Algebra/package.dhall

let DimensionalityEncoderExp = ./DimensionalityEncoderExp.dhall

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
          ++  ") >\$< "
          ++  ( if    params.nullable
                then  "Encoders.nullable"
                else  "Encoders.nonNullable"
              )
          ++  " ("
          ++  DimensionalityEncoderExp.run
                { dimensionality = params.dimensionality
                , elementIsNullable = params.elementIsNullable
                , elementExp = "scalarEncoder"
                }
          ++  ")"
      )

let Algebra = ../Algebras/Template/package.dhall

let Prelude = ../Prelude.dhall

let Lude = ../Lude.dhall

let Params =
      { dimensionality : Natural, elementIsNullable : Bool, elementExp : Text }

let run =
      \(params : Params) ->
        if    Natural/isZero params.dimensionality
        then  params.elementExp
        else  let base =
                    if    params.elementIsNullable
                    then  "Encoders.nullable ${params.elementExp}"
                    else  "Encoders.nonNullable ${params.elementExp}"

              let base = "Encoders.element (${base})"

              let arrayExp =
                    Natural/fold
                      params.dimensionality
                      Text
                      ( \(inner : Text) ->
                          "Encoders.dimension Vector.foldl' (${inner})"
                      )
                      base

              in  "Encoders.array (${arrayExp})"

in  { Params, run }

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
                    then  "Decoders.nullable ${params.elementExp}"
                    else  "Decoders.nonNullable ${params.elementExp}"

              let base = "Decoders.element (${base})"

              let arrayExp =
                    Natural/fold
                      params.dimensionality
                      Text
                      ( \(inner : Text) ->
                          "Decoders.dimension Vector.replicateM (${inner})"
                      )
                      base

              in  "Decoders.array (${arrayExp})"

in  { Params, run }

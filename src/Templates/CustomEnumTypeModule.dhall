let Algebra = ./Algebra/package.dhall

let Deps = ../Deps/package.dhall

let Variant = { name : Text, pgValue : Text }

let Params =
      { preludeModuleName : Text
      , moduleName : Text
      , typeName : Text
      , pgSchemaName : Text
      , pgTypeName : Text
      , variants : List Variant
      }

let run =
      \(params : Params) ->
        ''
        module ${params.moduleName} where

        import ${params.preludeModuleName}
        import Hasql.Mapping.Scalar

        -- |
        -- Representation of the @${params.pgTypeName}@ user-declared PostgreSQL enumeration type.
        data ${params.typeName}
          = ${Deps.Lude.Extensions.Text.indent
                2
                ( Deps.Prelude.Text.concatMapSep
                    ''

                    | ''
                    Variant
                    ( \(variant : Variant) ->
                        ''
                        -- | Corresponds to the PostgreSQL enum variant @${variant.pgValue}@.
                          ${variant.name}${params.typeName}''
                    )
                    params.variants
                )}
          deriving stock (Show, Eq, Ord, Enum, Bounded)

        instance IsScalar ${params.typeName} where
          scalarEncoder =
            Encoders.enum
              (Just "${params.pgSchemaName}")
              "${params.pgTypeName}"
              ( \case
                  ${Deps.Lude.Extensions.Text.indent
                      10
                      ( Deps.Prelude.Text.concatMapSep
                          "\n"
                          Variant
                          ( \(variant : Variant) ->
                              "${variant.name}${params.typeName} -> \"${variant.pgValue}\""
                          )
                          params.variants
                      )}
              )
          
          scalarDecoder =
            Decoders.enum
              (Just "${params.pgSchemaName}")
              "${params.pgTypeName}"
              ( \case
                  ${Deps.Lude.Extensions.Text.indent
                      10
                      ( Deps.Prelude.Text.concatMapSep
                          "\n"
                          Variant
                          ( \(variant : Variant) ->
                              "\"${variant.pgValue}\" -> Just ${variant.name}${params.typeName}"
                          )
                          params.variants
                      )}
                  _ -> Nothing
              )
        ''

in  { Params, Variant, run }

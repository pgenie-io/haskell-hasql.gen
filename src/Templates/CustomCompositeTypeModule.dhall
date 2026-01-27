let Algebra = ./Algebra/package.dhall

let Deps = ../Deps/package.dhall

let Params =
      { preludeModuleName : Text
      , moduleName : Text
      , typeName : Text
      , pgSchemaName : Text
      , pgTypeName : Text
      , fieldDeclarations : List Text
      , fieldEncoderExps : List Text
      , fieldDecoderExps : List Text
      }

let run =
      \(params : Params) ->
        ''
        module ${params.moduleName} where

        import ${params.preludeModuleName}
        import Hasql.Mapping.Scalar

        -- |
        -- Representation of the @${params.pgTypeName}@ user-declared PostgreSQL record type.
        data ${params.typeName} = ${params.typeName}
          { ${Deps.Lude.Extensions.Text.indent
                4
                ( Deps.Prelude.Text.concatSep
                    ''
                    ,
                    ''
                    params.fieldDeclarations
                )}
          }
          deriving stock (Show, Eq, Ord)

        instance IsScalar ${params.typeName} where
          scalarEncoder =
            Encoders.composite
              (Just "${params.pgSchemaName}")
              "${params.pgTypeName}"
              ( mconcat
                  [ ${Deps.Lude.Extensions.Text.indent
                        12
                        ( Deps.Prelude.Text.concatMapSep
                            ''
                            ,
                            ''
                            Text
                            (\(field : Text) -> "Encoders.field (${field})")
                            params.fieldEncoderExps
                        )}
                  ]
              )
          
          scalarDecoder =
            Decoders.composite
              (Just "${params.pgSchemaName}")
              "${params.pgTypeName}"
              ( ${params.typeName}
                  <$> ${Deps.Lude.Extensions.Text.indent
                          10
                          ( Deps.Prelude.Text.concatMapSep
                              ''

                              <*> ''
                              Text
                              (\(field : Text) -> "Decoders.field (${field})")
                              params.fieldDecoderExps
                          )}
              )
          
        ''

in  Algebra.module Params run

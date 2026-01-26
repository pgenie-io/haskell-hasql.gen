let Algebra = ./Algebra/package.dhall

let Prelude = ../Prelude.dhall

let Lude = ../Lude.dhall

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
          { ${Lude.Extensions.Text.indent
                4
                ( Prelude.Text.concatSep
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
                  [ ${Lude.Extensions.Text.indent
                        12
                        ( Prelude.Text.concatMapSep
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
                  <$> ${Lude.Extensions.Text.indent
                          10
                          ( Prelude.Text.concatMapSep
                              ''

                              <*> ''
                              Text
                              (\(field : Text) -> "Decoders.field (${field})")
                              params.fieldDecoderExps
                          )}
              )
          
        ''

in  Algebra.module Params run

let Algebra = ../Algebras/Template/package.dhall

let Prelude = ../Prelude.dhall

let Lude = ../Lude.dhall

let DimensionalityEncoderExp = ./DimensionalityEncoderExp.dhall

let DimensionalityDecoderExp = ./DimensionalityDecoderExp.dhall

let FieldEncoder = ./FieldEncoder.dhall

let Field =
      { name : Text, sig : Text, nullable : Bool, dimensionality : Natural }

let Params =
      { preludeModuleName : Text
      , moduleName : Text
      , typeName : Text
      , pgSchemaName : Text
      , pgTypeName : Text
      , fields : List Field
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
                ( Prelude.Text.concatMapSep
                    ''
                    ,
                    ''
                    Field
                    (\(field : Field) -> field.name ++ " :: " ++ field.sig)
                    params.fields
                )}
          }
          deriving stock (Show, Eq, Ord)

        instance IsScalar ${params.typeName} where
          valueEncoder :: Encoders.Value ${params.typeName}
          valueEncoder =
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
                            Field
                            ( \(field : Field) ->
                                    "Encoders.field ("
                                ++  FieldEncoder.run
                                      { name = field.name
                                      , nullable = field.nullable
                                      , dimensionality = field.dimensionality
                                      , elementIsNullable = True
                                      }
                                ++  ")"
                            )
                            params.fields
                        )}
                  ]
              )
          
          valueDecoder :: Decoders.Value ${params.typeName}
          valueDecoder =
            Decoders.composite
              (Just "${params.pgSchemaName}")
              "${params.pgTypeName}"
              ( ${params.typeName}
                  <$> ${Lude.Extensions.Text.indent
                          10
                          ( Prelude.Text.concatMapSep
                              ''

                              <*> ''
                              Field
                              ( \(field : Field) ->
                                      "Decoders.field ("
                                  ++  ( if    field.nullable
                                        then  "Decoders.nullable"
                                        else  "Decoders.nonNullable"
                                      )
                                  ++  " ("
                                  ++  DimensionalityDecoderExp.run
                                        { dimensionality = field.dimensionality
                                        , elementIsNullable = True
                                        , elementExp = "valueDecoder"
                                        }
                                  ++  "))"
                              )
                              params.fields
                          )}
              )
          
        ''

in  { Params, Field, run }

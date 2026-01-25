let Algebra = ../Algebra.dhall

let Sdk = Algebra.Sdk

let Lude = Algebra.Lude

let Model = Algebra.Model

let Value = ./Value.dhall

let Templates = ../../Templates/package.dhall

let Input = Model.Member

let Output =
      { fieldName : Text
      , sig : Text
      , encoderExp : Text
      , decoderExp : Text
      , customCompositeTypeModuleField :
          Templates.CustomCompositeTypeModule.Field
      , fieldEncoder : Text
      }

let run =
      \(config : Algebra.Config) ->
      \(input : Input) ->
        Sdk.Compiled.flatMap
          Value.Output
          Output
          ( \(value : Value.Output) ->
              let fieldName = Algebra.Name.toTextInCamel input.name

              let dimensionality =
                    merge
                      { Some =
                          \(arraySettings : Model.ArraySettings) ->
                            arraySettings.dimensionality
                      , None = 0
                      }
                      input.value.arraySettings

              in  if    input.isNullable
                  then  let sig = "Maybe ${value.sig}"

                        in  Sdk.Compiled.ok
                              Output
                              { fieldName
                              , sig
                              , encoderExp =
                                  "Encoders.nullable ${value.encoderExp}"
                              , decoderExp =
                                  "Decoders.nullable ${value.decoderExp}"
                              , customCompositeTypeModuleField =
                                { name = fieldName
                                , sig
                                , nullable = True
                                , dimensionality
                                }
                              , fieldEncoder =
                                  Templates.FieldEncoder.run
                                    { name = fieldName
                                    , nullable = True
                                    , dimensionality
                                    , elementIsNullable = True
                                    }
                              }
                  else  Sdk.Compiled.ok
                          Output
                          { fieldName
                          , sig = value.sig
                          , encoderExp =
                              "Encoders.nonNullable ${value.encoderExp}"
                          , decoderExp =
                              "Decoders.nonNullable ${value.decoderExp}"
                          , customCompositeTypeModuleField =
                            { name = fieldName
                            , sig = value.sig
                            , nullable = False
                            , dimensionality
                            }
                          , fieldEncoder =
                              Templates.FieldEncoder.run
                                { name = fieldName
                                , nullable = False
                                , dimensionality
                                , elementIsNullable = True
                                }
                          }
          )
          ( Sdk.Compiled.nest
              Value.Output
              input.pgName
              (Value.run config input.value)
          )

in  Algebra.module Input Output run

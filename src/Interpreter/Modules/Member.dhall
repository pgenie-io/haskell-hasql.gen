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
      , declaredCompositeTypeModuleField :
          Templates.DeclaredCompositeTypeModule.Field
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
                  then  Sdk.Compiled.ok
                          Output
                          { fieldName
                          , sig = "Maybe ${value.sig}"
                          , encoderExp = "Encoders.nullable ${value.encoderExp}"
                          , decoderExp = "Decoders.nullable ${value.decoderExp}"
                          , declaredCompositeTypeModuleField =
                            { name = fieldName
                            , sig = "Maybe ${value.sig}"
                            , nullable = True
                            , dimensionality
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
                          , declaredCompositeTypeModuleField =
                            { name = fieldName
                            , sig = value.sig
                            , nullable = False
                            , dimensionality
                            }
                          }
          )
          ( Sdk.Compiled.nest
              Value.Output
              input.pgName
              (Value.run config input.value)
          )

in  Algebra.module Input Output run

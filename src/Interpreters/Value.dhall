let Deps = ../Deps/package.dhall

let Algebra = ./Algebra/package.dhall

let Sdk = Deps.Sdk

let Model = Deps.Sdk.Project

let Templates = ../Templates/package.dhall

let Scalar = ./Scalar.dhall

let Input = Model.Value

let Output = { sig : Text, encoderExp : Text, decoderExp : Text }

let Result = Sdk.Compiled.Type Output

let run =
      \(config : Algebra.Config) ->
      \(input : Input) ->
        Sdk.Compiled.flatMap
          Scalar.Output
          Output
          ( \(scalar : Scalar.Output) ->
              Deps.Prelude.Optional.fold
                Model.ArraySettings
                input.arraySettings
                Result
                ( \(arraySettings : Model.ArraySettings) ->
                    Sdk.Compiled.ok
                      Output
                      { sig =
                          Templates.DimensionalityType.run
                            { dimensionality = arraySettings.dimensionality
                            , elementIsNullable =
                                arraySettings.elementIsNullable
                            , elementSig = scalar.sig
                            }
                      , encoderExp =
                          Templates.DimensionalityEncoderExp.run
                            { dimensionality = arraySettings.dimensionality
                            , elementIsNullable =
                                arraySettings.elementIsNullable
                            , elementExp = scalar.encoderExp
                            }
                      , decoderExp =
                          Templates.DimensionalityDecoderExp.run
                            { dimensionality = arraySettings.dimensionality
                            , elementIsNullable =
                                arraySettings.elementIsNullable
                            , elementExp = scalar.decoderExp
                            }
                      }
                )
                ( Sdk.Compiled.ok
                    Output
                    { sig = scalar.sig
                    , encoderExp = scalar.encoderExp
                    , decoderExp = scalar.decoderExp
                    }
                )
          )
          (Scalar.run config input.scalar)

in  Algebra.module Input Output run

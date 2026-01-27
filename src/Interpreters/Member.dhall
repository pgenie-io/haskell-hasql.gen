let Deps = ../Deps/package.dhall

let Algebra = ./Algebra/package.dhall

let Sdk = Deps.Sdk

let Model = Deps.Sdk.Project

let Value = ./Value.dhall

let Templates = ../Templates/package.dhall

let Input = Model.Member

let Output =
      { fieldName : Text
      , fieldDeclaration : Text
      , fieldEncoder : Text -> Text
      , fieldDecoder : Text
      }

let run =
      \(config : Algebra.Config) ->
      \(input : Input) ->
        Sdk.Compiled.flatMap
          Value.Output
          Output
          ( \(value : Value.Output) ->
              let fieldName = Deps.CodegenKit.Name.toTextInCamel input.name

              let dimensionality =
                    merge
                      { Some =
                          \(arraySettings : Model.ArraySettings) ->
                            arraySettings.dimensionality
                      , None = 0
                      }
                      input.value.arraySettings

              let elementIsNullable =
                    merge
                      { Some =
                          \(arraySettings : Model.ArraySettings) ->
                            arraySettings.elementIsNullable
                      , None = True
                      }
                      input.value.arraySettings

              let sig = value.sig

              let sig = if input.isNullable then "Maybe (${sig})" else sig

              in  Sdk.Compiled.ok
                    Output
                    { fieldName
                    , fieldEncoder =
                        \(surroundingEncoder : Text) ->
                          Templates.FieldEncoder.run
                            { name = fieldName
                            , nullable = input.isNullable
                            , dimensionality
                            , elementIsNullable
                            , surroundingEncoder
                            }
                    , fieldDecoder =
                        Templates.FieldDecoder.run
                          { nullable = input.isNullable
                          , dimensionality
                          , elementIsNullable
                          }
                    , fieldDeclaration =
                        Templates.FieldDeclaration.run
                          { name = fieldName, sig, docs = None Text }
                    }
          )
          ( Sdk.Compiled.nest
              Value.Output
              input.pgName
              (Value.run config input.value)
          )

in  Algebra.module Input Output run

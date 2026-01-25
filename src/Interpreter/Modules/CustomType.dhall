let Algebra = ../Algebra.dhall

let Sdk = Algebra.Sdk

let Lude = Algebra.Lude

let Model = Algebra.Model

let Templates = ../../Templates/package.dhall

let Input = Model.CustomType

let Output =
      { moduleName : Text
      , moduleNamespace : Text
      , modulePath : Text
      , moduleContent : Text
      }

in  Algebra.module
      Input
      Output
      ( \(config : Algebra.Config) ->
        \(input : Input) ->
          let moduleName = Algebra.Name.toTextInPascal input.name

          let moduleNamespaceAsList =
                  config.rootNamespace
                # [ "DeclaredTypes", Algebra.Name.toTextInPascal input.name ]

          let moduleNamespace =
                Algebra.Prelude.Text.concatSep "." moduleNamespaceAsList

          let modulePath =
                Templates.ModulePath.run { namespace = moduleNamespaceAsList }

          in  merge
                { Composite =
                    \(members : List Model.Member) ->
                      Sdk.Compiled.ok
                        Output
                        { moduleName
                        , moduleNamespace
                        , modulePath
                        , moduleContent =
                            Templates.DeclaredCompositeTypeModule.run
                              { moduleName = moduleNamespace
                              , typeName = moduleName
                              , pgSchemaName = input.pgSchemaName
                              , pgTypeName = input.pgName
                              , fields =
                                  Algebra.Prelude.List.map
                                    Model.Member
                                    Templates.DeclaredCompositeTypeModule.Field
                                    ( \(member : Model.Member) ->
                                        { name =
                                            Algebra.Name.toTextInCamel
                                              member.name
                                        , sig = "TODO: sig"
                                        , nullable = member.isNullable
                                        , dimensionality =
                                            merge
                                              { None = 0
                                              , Some =
                                                  \ ( arraySettings
                                                    : Model.ArraySettings
                                                    ) ->
                                                    arraySettings.dimensionality
                                              }
                                              member.value.arraySettings
                                        }
                                    )
                                    members
                              }
                        }
                , Enum =
                    \(variants : List Model.EnumVariant) ->
                      Sdk.Compiled.ok
                        Output
                        { moduleName
                        , moduleNamespace
                        , modulePath
                        , moduleContent =
                            Templates.DeclaredEnumTypeModule.run
                              { moduleName = moduleNamespace
                              , typeName = moduleName
                              , pgSchemaName = input.pgSchemaName
                              , pgTypeName = input.pgName
                              , variants =
                                  Algebra.Prelude.List.map
                                    Model.EnumVariant
                                    Templates.DeclaredEnumTypeModule.Variant
                                    ( \(variant : Model.EnumVariant) ->
                                        { name =
                                            Algebra.Name.toTextInPascal
                                              variant.name
                                        , pgValue = variant.pgName
                                        }
                                    )
                                    variants
                              }
                        }
                , Domain =
                    \(value : Model.Value) ->
                      Sdk.Compiled.message
                        Output
                        "Domain types are not yet supported."
                }
                input.definition
      )

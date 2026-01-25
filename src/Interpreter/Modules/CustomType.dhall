let Algebra = ../Algebra.dhall

let Sdk = Algebra.Sdk

let Lude = Algebra.Lude

let Model = Algebra.Model

let Templates = ../../Templates/package.dhall

let MemberGen = ./Member.dhall

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
                      let compiledMembers
                          : Sdk.Compiled.Type (List MemberGen.Output)
                          = Sdk.Compiled.traverseList
                              Model.Member
                              MemberGen.Output
                              (MemberGen.run config)
                              members

                      let compiledOutput
                          : Sdk.Compiled.Type Output
                          = Sdk.Compiled.map
                              (List MemberGen.Output)
                              Output
                              ( \(members : List MemberGen.Output) ->
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
                                              MemberGen.Output
                                              Templates.DeclaredCompositeTypeModule.Field
                                              ( \(member : MemberGen.Output) ->
                                                  member.declaredCompositeTypeModuleField
                                              )
                                              members
                                        }
                                  }
                              )
                              compiledMembers

                      in  compiledOutput
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

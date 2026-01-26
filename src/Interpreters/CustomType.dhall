let Algebra = ./Algebra/package.dhall

let Sdk = Algebra.Sdk

let Model = Algebra.Model

let Templates = ../Templates/package.dhall

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
                # [ "CustomTypes", Algebra.Name.toTextInPascal input.name ]

          let moduleNamespace =
                Algebra.Prelude.Text.concatSep "." moduleNamespaceAsList

          let modulePath =
                Templates.ModulePath.run { namespace = moduleNamespaceAsList }

          let preludeModuleName =
                Algebra.Prelude.Text.concatSep
                  "."
                  (config.rootNamespace # [ "Preludes", "CustomType" ])

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
                                      Templates.CustomCompositeTypeModule.run
                                        { preludeModuleName
                                        , moduleName = moduleNamespace
                                        , typeName = moduleName
                                        , pgSchemaName = input.pgSchemaName
                                        , pgTypeName = input.pgName
                                        , fieldDeclarations =
                                            Algebra.Prelude.List.map
                                              MemberGen.Output
                                              Text
                                              ( \(member : MemberGen.Output) ->
                                                  member.fieldDeclaration
                                              )
                                              members
                                        , fieldEncoderExps =
                                            Algebra.Prelude.List.map
                                              MemberGen.Output
                                              Text
                                              ( \(member : MemberGen.Output) ->
                                                  member.fieldEncoder
                                              )
                                              members
                                        , fieldDecoderExps =
                                            Algebra.Prelude.List.map
                                              MemberGen.Output
                                              Text
                                              ( \(member : MemberGen.Output) ->
                                                  member.fieldDecoder
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
                            Templates.CustomEnumTypeModule.run
                              { preludeModuleName
                              , moduleName = moduleNamespace
                              , typeName = moduleName
                              , pgSchemaName = input.pgSchemaName
                              , pgTypeName = input.pgName
                              , variants =
                                  Algebra.Prelude.List.map
                                    Model.EnumVariant
                                    Templates.CustomEnumTypeModule.Variant
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

let Deps = ../Deps/package.dhall

let Algebra = ./Algebra/package.dhall

let Lude = Deps.Lude

let Typeclasses = Deps.Typeclasses

let Sdk = Deps.Sdk

let Templates = ../Templates/package.dhall

let ResultModule = ./Result.dhall

let QueryFragmentsModule = ./QueryFragments.dhall

let MemberModule = ./Member.dhall

let Input = Deps.Sdk.Project.Query

let Output =
      { statementModuleName : Text
      , statementModuleNamespace : Text
      , statementModulePath : Text
      , statementModuleContents : Text
      }

let render =
      \(config : Algebra.Config) ->
      \(input : Input) ->
      \(result : ResultModule.Output) ->
      \(fragments : QueryFragmentsModule.Output) ->
      \(params : List MemberModule.Output) ->
        let statementModuleName = Deps.CodegenKit.Name.toTextInPascal input.name

        let statementModuleNamespaceAsList =
              config.rootNamespace # [ "Statements", statementModuleName ]

        let statementModuleNamespace =
              Deps.Prelude.Text.concatSep "." statementModuleNamespaceAsList

        let statementModulePath =
              Templates.ModulePath.run
                { namespace = statementModuleNamespaceAsList }

        let statementTypeName = statementModuleName

        let statementResultTypeName = statementModuleName ++ "Result"

        let result = result statementModuleName

        let projectNamespace =
              Deps.Prelude.Text.concatSep "." config.rootNamespace

        let statementModuleContents =
              ''
              module ${statementModuleNamespace} where

              import ${projectNamespace}.Prelude
              import qualified Hasql.Statement as Statement
              import qualified Hasql.Decoders as Decoders
              import qualified Hasql.Encoders as Encoders
              import qualified Data.Aeson as Aeson
              import qualified Data.Vector as Vector
              import qualified Hasql.Mapping as Mapping
              import qualified ${projectNamespace}.CustomTypes as CustomTypes

              ${Templates.ParamsTypeDecl.run
                  { queryName = Deps.CodegenKit.Name.toTextInSnake input.name
                  , sqlForDocs = fragments.haddock
                  , srcPath = input.srcPath
                  , typeName = statementTypeName
                  , fields =
                      Deps.Prelude.List.map
                        MemberModule.Output
                        Text
                        ( \(member : MemberModule.Output) ->
                            member.fieldDeclaration
                        )
                        params
                  }}

              ${Deps.Prelude.Text.concatSep "\n\n" result.typeDecls}

              instance Mapping.IsStatement ${statementTypeName} where
                type Result ${statementTypeName} = ${statementResultTypeName}

                statement = Statement.preparable sql encoder decoder
                  where
                    sql =
                      ${Deps.Lude.Extensions.Text.indent 8 fragments.exp}

                    encoder =
                      mconcat
                        [ ${Lude.Extensions.Text.indent
                              12
                              ( Deps.Prelude.Text.concatMapSep
                                  ''
                                  ,
                                  ''
                                  MemberModule.Output
                                  ( \(member : MemberModule.Output) ->
                                      member.fieldEncoder "Encoders.param"
                                  )
                                  params
                              )}
                        ]

                    decoder =
                      ${Deps.Lude.Extensions.Text.indent 8 result.decoderExp}

              ''

        in  { statementModuleName
            , statementModuleNamespace
            , statementModulePath
            , statementModuleContents
            }

let run =
      \(config : Algebra.Config) ->
      \(input : Input) ->
        Sdk.Compiled.nest
          Output
          input.srcPath
          ( Typeclasses.Classes.Applicative.map3
              Sdk.Compiled.Type
              Sdk.Compiled.applicative
              ResultModule.Output
              QueryFragmentsModule.Output
              (List MemberModule.Output)
              Output
              (render config input)
              ( Sdk.Compiled.nest
                  ResultModule.Output
                  "result"
                  (ResultModule.run config input.result)
              )
              ( Sdk.Compiled.nest
                  QueryFragmentsModule.Output
                  "sql"
                  (QueryFragmentsModule.run config input.fragments)
              )
              ( Sdk.Compiled.nest
                  (List MemberModule.Output)
                  "params"
                  ( Typeclasses.Classes.Applicative.traverseList
                      Sdk.Compiled.Type
                      Sdk.Compiled.applicative
                      Deps.Sdk.Project.Member
                      MemberModule.Output
                      (MemberModule.run config)
                      input.params
                  )
              )
          )

in  Algebra.module Input Output run

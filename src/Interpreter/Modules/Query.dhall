let Algebra = ../Algebra.dhall

let Lude = Algebra.Lude

let Typeclasses = Algebra.Typeclasses

let Sdk = Algebra.Sdk

let Templates = ../../Templates/package.dhall

let ResultModule = ./Result.dhall

let QueryFragmentsModule = ./QueryFragments.dhall

let MemberModule = ./Member.dhall

let Input = Algebra.Model.Query

let Output =
      forall (projectNamespace : List Text) ->
        { statementModuleName : Text
        , statementModuleNamespace : Text
        , statementModulePath : Text
        , statementModuleContents : Text
        }

let render =
      \(input : Input) ->
      \(result : ResultModule.Output) ->
      \(fragments : QueryFragmentsModule.Output) ->
      \(paramsMembers : List MemberModule.Output) ->
      \(projectNamespace : List Text) ->
        let statementModuleName = Algebra.Name.toTextInPascal input.name

        let statementModuleNamespaceAsList =
              projectNamespace # [ "Statements", statementModuleName ]

        let statementModuleNamespace =
              Algebra.Prelude.Text.concatSep "." statementModuleNamespaceAsList

        let statementModulePath =
              Templates.ModulePath.run
                { namespace = statementModuleNamespaceAsList }

        let statementTypeName = statementModuleName

        let statementResultTypeName = statementModuleName ++ "Result"

        let result = result statementModuleName

        let statementModuleContents =
              ''
              module ${statementModuleNamespace} where

              import qualified Hasql.Statement as Statement
              import qualified Hasql.Decoders as Decoders
              import qualified Hasql.Encoders as Encoders
              import qualified Data.ByteString as ByteString
              import qualified Data.Int as Int
              import qualified Data.Text as Text
              import qualified Data.Vector as Vector

              ${Templates.ParamsTypeDecl.run
                  { queryName = Algebra.Name.toTextInSnake input.name
                  , sqlForDocs = fragments.haddock
                  , srcPath = input.srcPath
                  , typeName = statementTypeName
                  , members =
                      Algebra.Prelude.List.map
                        MemberModule.Output
                        { fieldName : Text, sig : Text }
                        ( \(member : MemberModule.Output) ->
                            { fieldName = member.fieldName, sig = member.sig }
                        )
                        paramsMembers
                  }}

              ${Algebra.Prelude.Text.concatSep "\n\n" result.typeDecls}

              instance IsStatement ${statementTypeName} where
                type ResultOf ${statementTypeName} = ${statementResultTypeName}

                statementOf = Statement.prepared sql encoder decoder
                  where
                    sql =
                      ${Algebra.Lude.Extensions.Text.indent 8 fragments.exp}

                    encoder =
                      error "TODO"

                    decoder =
                      ${Algebra.Lude.Extensions.Text.indent 8 result.decoderExp}

              ''

        in  { statementModuleName
            , statementModuleNamespace
            , statementModulePath
            , statementModuleContents
            }

let run
    : Input -> Sdk.Compiled.Type Output
    = \(input : Input) ->
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
              (render input)
              ( Sdk.Compiled.nest
                  ResultModule.Output
                  "result"
                  (ResultModule.run input.result)
              )
              ( Sdk.Compiled.nest
                  QueryFragmentsModule.Output
                  "sql"
                  (QueryFragmentsModule.run input.fragments)
              )
              ( Sdk.Compiled.nest
                  (List MemberModule.Output)
                  "params"
                  ( Typeclasses.Classes.Applicative.traverseList
                      Sdk.Compiled.Type
                      Sdk.Compiled.applicative
                      Algebra.Model.Member
                      MemberModule.Output
                      MemberModule.run
                      input.params
                  )
              )
          )

in  Algebra.module Input Output run

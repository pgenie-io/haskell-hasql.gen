let Algebra = ../Algebra.dhall

let Lude = Algebra.Lude

let Typeclasses = Algebra.Typeclasses

let Sdk = Algebra.Sdk

let ResultModule = ./Result.dhall

let QueryFragmentsModule = ./QueryFragments.dhall

let MemberModule = ./Member.dhall

let Input = Algebra.Model.Query

let Output =
      forall (projectNamespace : List Text) ->
        { statementModuleNamespace : Text
        , statementModulePath : Text
        , statementModuleContents : Text
        }

let renderParamsTypeDecl
    : Algebra.Model.Query -> Text -> List MemberModule.Output -> Text
    = \(query : Algebra.Model.Query) ->
      \(sqlForDocs : Text) ->
      \(members : List MemberModule.Output) ->
        let paramsTypeName = Algebra.Name.toTextInPascal query.name ++ "Params"

        let fieldDecls =
              Algebra.Prelude.Text.concatMapSep
                ("\n" ++ ",")
                MemberModule.Output
                ( \(member : MemberModule.Output) ->
                    member.fieldName ++ " :: " ++ member.sig
                )
                members

        in  ''
            -- |
            -- Parameters for the @${Algebra.Name.toTextInSnake
                                       query.name}@ query.
            --
            -- == SQL Template
            --
            -- > ${Algebra.Lude.Extensions.Text.prefixEachLine
                     "-- > "
                     sqlForDocs}
            --
            -- == Source Path
            --
            -- > ${query.srcPath}
            --
            data ${paramsTypeName} = ${paramsTypeName}
              { ${Algebra.Lude.Extensions.Text.indent 4 fieldDecls}
              }
              deriving stock (Eq, Show)
            ''

let render =
      \(input : Input) ->
      \(result : ResultModule.Output) ->
      \(fragments : QueryFragmentsModule.Output) ->
      \(paramsMembers : List MemberModule.Output) ->
      \(projectNamespace : List Text) ->
        let statementModuleName = Algebra.Name.toTextInPascal input.name

        let statementModuleNamespace =
              Algebra.Prelude.Text.concatSep
                "."
                (projectNamespace # [ statementModuleName ])

        let statementModulePath =
                  Algebra.Prelude.Text.concatSep
                    "/"
                    (projectNamespace # [ statementModuleName ])
              ++  ".hs"

        let result = result statementModuleName

        let statementModuleContents =
              ''
              module ${statementModuleNamespace} where

              import Hasql.Statement (Statement (..))
              import qualified Hasql.Decoders as Decoders
              import qualified Hasql.Encoders as Encoders
              import qualified Data.ByteString as ByteString
              import qualified Data.Int as Int
              import qualified Data.Text as Text
              import qualified Data.Vector as Vector

              ${Algebra.Prelude.Text.concatSep "\n\n" result.typeDecls}

              statement : Statement ${statementModuleName}Params ${statementModuleName} Result
              statement =
                Statement sql encoder decoder True
                where
                  sql =
                    ${Algebra.Lude.Extensions.Text.indent 6 fragments.exp}

                  encoder =
                    error "TODO"

                  decoder = ${Algebra.Lude.Extensions.Text.indent
                                4
                                result.decoderExp}

              ${renderParamsTypeDecl input fragments.haddock paramsMembers}

              sql :: Text.Text
              sql =
                ${Algebra.Lude.Extensions.Text.indent 2 fragments.exp}
              ''

        in  { statementModuleNamespace
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

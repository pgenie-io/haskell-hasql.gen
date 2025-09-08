let Algebra = ../Algebra.dhall

let Lude = Algebra.Lude

let Sdk = Algebra.Sdk

let ResultModule = ./Result.dhall

let QueryFragmentsModule = ./QueryFragments.dhall

let Input = Algebra.Model.Query

let Output =
      forall (projectNamespace : List Text) ->
        { statementModuleNamespace : Text
        , statementModulePath : Text
        , statementModuleContents : Text
        }

let render =
      \(input : Input) ->
      \(result : ResultModule.Output) ->
      \(fragments : QueryFragmentsModule.Output) ->
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
                    ${fragments.exp}

                  encoder =
                    error "TODO"

                  decoder = ${Algebra.Lude.Extensions.Text.indent
                                4
                                result.decoderExp}
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
          ( Lude.Algebras.Applicative.map2
              Sdk.Compiled.Type
              Sdk.Compiled.applicative
              ResultModule.Output
              QueryFragmentsModule.Output
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
          )

in  Algebra.module Input Output run

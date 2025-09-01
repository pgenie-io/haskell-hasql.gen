let Algebra = ../Algebra.dhall

let ResultModule = ./Result.dhall

let Input = Algebra.Model.Query

let Output =
      forall (projectNamespace : List Text) ->
        { statementModuleNamespace : Text
        , statementModulePath : Text
        , statementModuleContents : Text
        }

let run
    : Input -> Algebra.Sdk.Compiled.Type Output
    = \(input : Input) ->
        Algebra.Sdk.Compiled.map
          ResultModule.Output
          Output
          ( \(result : ResultModule.Output) ->
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
                          error "TODO"

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
          )
          ( Algebra.Sdk.Compiled.nest
              ResultModule.Output
              input.srcPath
              (ResultModule.run input.result)
          )

in  Algebra.module Input Output run

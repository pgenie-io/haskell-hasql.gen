let Prelude = ./Prelude.dhall

let Lude = ./Lude.dhall

let Sdk = ./Sdk.dhall

let CodegenKit = ./CodegenKit.dhall

let Gen = Sdk.Gen

let Model = Sdk.Project

let Fixtures = Sdk.Fixtures

let fixture1 = Fixtures._1

let Package = ./Package/package.dhall

let Name = CodegenKit.Name

let compiled = Package.compile fixture1

let renderValueError
    : Gen.ValueError -> Text
    = \(valueError : Gen.ValueError) ->
        merge
          { UnsupportedPrimitive =
              \(primitive : Model.Primitive) ->
                "Unsupported type: " ++ Model.Primitive/toText primitive
          , UnsupportedDimensionality =
              \(dimensionality : Natural) ->
                    "Unsupported dimensionality: "
                ++  Prelude.Natural.show dimensionality
          , Custom = \(error : Text) -> "Custom error: " ++ error
          }
          valueError

let renderWarning
    : Gen.Warning -> Text
    = \(warning : Gen.Warning) ->
        merge
          { QuerySkipped =
              \ ( querySkipped
                : { query : Model.Query, dueTo : Gen.QueryError }
                ) ->
                ''
                Query skipped: ${querySkipped.query.srcPath}
                ${merge
                    { Param =
                        \(param : Gen.FieldError) ->
                          ''
                          In param: ${Name.toTextInSnake param.name}
                          ${renderValueError param.dueTo}''
                    , ResultColumn =
                        \(resultColumn : Gen.FieldError) ->
                          ''
                          In result column: ${Name.toTextInSnake
                                                resultColumn.name}
                          ${renderValueError resultColumn.dueTo}''
                    , GeneratorFailure =
                        \(text : Text) -> "Generator failure: ${text}"
                    }
                    querySkipped.dueTo}''
          , GeneratorWarning = \(text : Text) -> "Generator warning: " ++ text
          }
          warning

in  ''
    Files:
      ${Lude.Extensions.Text.indent
          2
          ( Prelude.Text.concatMap
              Gen.File
              ( \(file : Gen.File) ->
                  ''
                  ${file.path}:
                    ${Lude.Extensions.Text.indent 2 file.content}
                    ---

                  ''
              )
              compiled.files
          )}

    Warnings:
      ${Lude.Extensions.Text.indent
          2
          ( Prelude.Text.concatMapSep
              "\n"
              Gen.Warning
              ( \(warning : Gen.Warning) ->
                  "- ${Lude.Extensions.Text.indent 2 (renderWarning warning)}"
              )
              compiled.warnings
          )}''

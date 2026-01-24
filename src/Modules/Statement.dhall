let Prelude = ../Prelude.dhall

let Lude = ../Lude.dhall

let Prelude = Prelude // { Text = Prelude.Text // Lude.Extensions.Text }

let Sdk = ../Sdk.dhall

let CodegenKit = ../CodegenKit.dhall

let Model = Sdk.Project

let Name = CodegenKit.Name

let Input = { projectNamespace : Text, query : Model.Query }

let Output = { namespace : Text, path : Text, content : Text }

let Snippets = ../Snippets/package.dhall

let Result = Lude.Structures.Result

let primitiveSig
    : Model.Primitive -> Optional Text
    = \(primitive : Model.Primitive) ->
        merge
          { Bool = Some "Bool"
          , Bytea = Some "ByteString"
          , Char = Some "Char"
          , Cidr = None Text
          , Date = Some "Day"
          , Datemultirange = None Text
          , Daterange = None Text
          , Float4 = Some "Float"
          , Float8 = Some "Double"
          , Inet = None Text
          , Int2 = Some "Int16"
          , Int4 = Some "Int32"
          , Int4multirange = None Text
          , Int4range = None Text
          , Int8 = Some "Int64"
          , Int8multirange = None Text
          , Int8range = None Text
          , Interval = Some "DiffTime"
          , Json = Some "Data.Aeson.Value"
          , Jsonb = Some "Data.Aeson.Value"
          , Macaddr = None Text
          , Macaddr8 = None Text
          , Money = None Text
          , Numeric = Some "Scientific"
          , Nummultirange = None Text
          , Numrange = None Text
          , Text = Some "Text"
          , Time = Some "TimeOfDay"
          , Timestamp = Some "LocalTime"
          , Timestamptz = Some "UTCTime"
          , Timetz = None Text
          , Tsmultirange = None Text
          , Tsrange = None Text
          , Tstzmultirange = None Text
          , Tstzrange = None Text
          , Uuid = Some "UUID"
          , Xml = None Text
          }
          primitive

let customSig
    : Name.Type -> Text
    = \(name : Name.Type) -> "CustomTypes." ++ Name.toTextInPascal name

let scalarSig
    : Model.Scalar -> Result.Type Model.Primitive Text
    = let AdaptedResult = Result.Type Model.Primitive Text

      in  \(scalar : Model.Scalar) ->
            merge
              { Primitive =
                  \(primitive : Model.Primitive) ->
                    merge
                      { None = AdaptedResult.Failure primitive
                      , Some = AdaptedResult.Success
                      }
                      (primitiveSig primitive)
              , Custom =
                  \(name : Name.Type) -> AdaptedResult.Success (customSig name)
              }
              scalar

let dimensionalSig
    : Model.Dimensional -> Result.Type Model.Primitive Text
    = \(dimensional : Model.Dimensional) ->
        let prefix =
              Prelude.Text.replicate dimensional.dimensionality "Vector ("

        let suffix = Prelude.Text.replicate dimensional.dimensionality ")"

        let sig = scalarSig dimensional.scalar

        in  Lude.Structures.Result.mapSuccess
              Model.Primitive
              Text
              Text
              (\(sig : Text) -> Prelude.Text.concat [ prefix, sig, suffix ])
              sig

let valueSig
    : Model.Value -> Result.Type Sdk.Gen.ValueError Text
    = \(value : Model.Value) ->
        let applyOptionality =
              if    value.isNullable
              then  \(sig : Text) -> "Maybe (" ++ sig ++ ")"
              else  Prelude.Function.identity Text

        in  Lude.Structures.Result.mapBoth
              Model.Primitive
              Sdk.Gen.ValueError
              Text
              Text
              ( \(error : Model.Primitive) ->
                  Sdk.Gen.ValueError.UnsupportedPrimitive error
              )
              applyOptionality
              (dimensionalSig value.dimensional)

let fieldSig
    : Model.Field -> Result.Type Sdk.Gen.FieldError Text
    = \(field : Model.Field) ->
        Result.mapError
          Sdk.Gen.ValueError
          Sdk.Gen.FieldError
          Text
          ( \(error : Sdk.Gen.ValueError) ->
              { name = field.name, dueTo = error }
          )
          (valueSig field.value)

let resultRowDecl
    : Text ->
      Prelude.NonEmpty.Type Model.Field ->
        Result.Type Sdk.Gen.FieldError Text
    = \(moduleName : Text) ->
      \(fields : Prelude.NonEmpty.Type Model.Field) ->
        Result.mapSuccess
          Sdk.Gen.FieldError
          (List Text)
          Text
          ( \(renderedFieldDecls : List Text) ->
              ''
              data ${moduleName}ResultRow = ${moduleName}ResultRow
                { ${Prelude.Text.indent
                      4
                      ( Prelude.Text.concatSep
                          ''
                          ,
                          ''
                          renderedFieldDecls
                      )}
                }
                deriving stock (Eq, Show)''
          )
          (   Result.traverseList
                Sdk.Gen.FieldError
                Model.Field
                Text
                ( \(field : Model.Field) ->
                    Result.mapBoth
                      Sdk.Gen.ValueError
                      Sdk.Gen.FieldError
                      Text
                      Text
                      ( \(dueTo : Sdk.Gen.ValueError) ->
                          { name = field.name, dueTo }
                      )
                      ( \(sig : Text) ->
                          "${Name.toTextInCamel field.name} :: ${sig}"
                      )
                      (valueSig field.value)
                )
                (Prelude.NonEmpty.toList Model.Field fields)
            : Result.Type Sdk.Gen.FieldError (List Text)
          )

let resultDecls
    : Text -> Optional Model.ResultRows -> Result.Type Sdk.Gen.FieldError Text
    = \(moduleName : Text) ->
      \(optional : Optional Model.ResultRows) ->
        merge
          { None =
              (Result.Type Sdk.Gen.FieldError Text).Success
                "type ${moduleName}Result = ()"
          , Some =
              \(resultRows : Model.ResultRows) ->
                Result.mapSuccess
                  Sdk.Gen.FieldError
                  Text
                  Text
                  ( \(resultRowDecls : Text) ->
                      merge
                        { Optional =
                            ''
                            type ${moduleName}Result = Maybe ${moduleName}ResultRow

                            ${resultRowDecls}
                            ''
                        , Single =
                            ''
                            type ${moduleName}Result = ${moduleName}ResultRow

                            ${resultRowDecls}
                            ''
                        , Multiple =
                            ''
                            type ${moduleName}Result = Vector ${moduleName}ResultRow

                            ${resultRowDecls}
                            ''
                        }
                        resultRows.category
                  )
                  (resultRowDecl moduleName resultRows.row)
          }
          optional

let resultDecoder
    : Optional Model.ResultRows -> Text
    = \(optional : Optional Model.ResultRows) ->
        merge
          { None = "Decoders.noResult"
          , Some = \(resultRows : Model.ResultRows) -> "TODO"
          }
          optional

let compile
    : Input -> Result.Type Sdk.Gen.QueryError Output
    = \(input : Input) ->
        let projectNamespace = input.projectNamespace

        let moduleName = Name.toTextInPascal input.query.name

        let namespace = "${projectNamespace}.Statements.${moduleName}"

        let paramsDecls
            : Result.Type Sdk.Gen.QueryError Text
            = Result.mapBoth
                Sdk.Gen.FieldError
                Sdk.Gen.QueryError
                (List { name : Text, sig : Text })
                Text
                Sdk.Gen.QueryError.Param
                ( \(fields : List { name : Text, sig : Text }) ->
                    ''
                    ${Snippets.recordDataDecl { name = moduleName, fields }}
                      deriving stock (Eq, Show)''
                )
                ( Result.traverseList
                    Sdk.Gen.FieldError
                    Model.Field
                    { name : Text, sig : Text }
                    ( \(field : Model.Field) ->
                        Result.mapBoth
                          Sdk.Gen.ValueError
                          Sdk.Gen.FieldError
                          Text
                          { name : Text, sig : Text }
                          ( \(dueTo : Sdk.Gen.ValueError) ->
                              { name = field.name, dueTo }
                          )
                          ( \(sig : Text) ->
                              { name = Name.toTextInCamel field.name, sig }
                          )
                          (valueSig field.value)
                    )
                    input.query.params
                )

        let resultDecls
            : Result.Type Sdk.Gen.QueryError Text
            = Result.mapError
                Sdk.Gen.FieldError
                Sdk.Gen.QueryError
                Text
                Sdk.Gen.QueryError.ResultColumn
                (resultDecls moduleName input.query.result)

        in  Result.mapSuccess2
              Sdk.Gen.QueryError
              Text
              Text
              Output
              ( \(paramsDecls : Text) ->
                \(resultDecls : Text) ->
                  { namespace
                  , path = "${projectNamespace}/Statements/${moduleName}.hs"
                  , content =
                      ''
                      module ${namespace} 
                        ( ${moduleName} (..),
                          ${moduleName}Result (..),
                        )
                      where

                      import Prelude
                      import Data.Text (Text)
                      import Data.UUID (UUID)
                      import Data.Vector (Vector)

                      import qualified Hasql.Decoders as Decoders
                      import qualified Hasql.Mapping as Mapping

                      ${paramsDecls}

                      ${resultDecls}
                      ''
                  }
              )
              paramsDecls
              resultDecls

in  { Input, Output, compile }

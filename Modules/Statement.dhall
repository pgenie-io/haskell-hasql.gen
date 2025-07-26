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

let primitiveSig
    : Model.Primitive -> Text
    = \(primitive : Model.Primitive) ->
        merge
          { Bool = "Bool"
          , Bytea = "ByteString"
          , Char = "Char"
          , Cidr = "Unknown"
          , Date = "Day"
          , Datemultirange = "Unknown"
          , Daterange = "Unknown"
          , Float4 = "Float"
          , Float8 = "Double"
          , Inet = "Unknown"
          , Int2 = "Int16"
          , Int4 = "Int32"
          , Int4multirange = "Unknown"
          , Int4range = "Unknown"
          , Int8 = "Int64"
          , Int8multirange = "Unknown"
          , Int8range = "Unknown"
          , Interval = "DiffTime"
          , Json = "Data.Aeson.Value"
          , Jsonb = "Data.Aeson.Value"
          , Macaddr = "Unknown"
          , Macaddr8 = "Unknown"
          , Money = "Unknown"
          , Numeric = "Scientific"
          , Nummultirange = "Unknown"
          , Numrange = "Unknown"
          , Text = "Text"
          , Time = "TimeOfDay"
          , Timestamp = "LocalTime"
          , Timestamptz = "UTCTime"
          , Timetz = "Unknown"
          , Tsmultirange = "Unknown"
          , Tsrange = "Unknown"
          , Tstzmultirange = "Unknown"
          , Tstzrange = "Unknown"
          , Uuid = "UUID"
          , Xml = "Unknown"
          }
          primitive

let customSig
    : Name.Type -> Text
    = \(name : Name.Type) -> "CustomTypes." ++ Name.toTextInPascal name

let scalarSig
    : Model.Scalar -> Text
    = \(scalar : Model.Scalar) ->
        merge { Primitive = primitiveSig, Custom = customSig } scalar

let dimensionalSig
    : Model.Dimensional -> Text
    = \(dimensional : Model.Dimensional) ->
        let prefix =
              Prelude.Text.replicate dimensional.dimensionality "Vector ("

        let suffix = Prelude.Text.replicate dimensional.dimensionality ")"

        let sig = scalarSig dimensional.scalar

        in  Prelude.Text.concat [ prefix, sig, suffix ]

let valueSig
    : Model.Value -> Text
    = \(value : Model.Value) ->
        let applyOptionality =
              if    value.isNullable
              then  \(sig : Text) -> "Maybe (" ++ sig ++ ")"
              else  Prelude.Function.identity Text

        in  applyOptionality (dimensionalSig value.dimensional)

let resultRowDecl
    : Text -> Prelude.NonEmpty.Type Model.Field -> Text
    = \(moduleName : Text) ->
      \(fields : Prelude.NonEmpty.Type Model.Field) ->
        ''
        data ${moduleName}ResultRow = ${moduleName}ResultRow
          { ${Prelude.Text.indent
                4
                ( Prelude.Text.concatMapSep
                    ''
                    ,
                    ''
                    Model.Field
                    ( \(field : Model.Field) ->
                        "${Name.toTextInCamel field.name} :: ${valueSig
                                                                 field.value}"
                    )
                    (Prelude.NonEmpty.toList Model.Field fields)
                )}
          }
          deriving stock (Eq, Show)
        ''

let resultDecls
    : Text -> Optional Model.ResultRows -> Text
    = \(moduleName : Text) ->
      \(optional : Optional Model.ResultRows) ->
        merge
          { None = "type ${moduleName}Result = ()"
          , Some =
              \(resultRows : Model.ResultRows) ->
                merge
                  { Optional =
                      ''
                      type ${moduleName}Result = Maybe ${moduleName}ResultRow

                      ${resultRowDecl moduleName resultRows.row}
                      ''
                  , Single =
                      ''
                      type ${moduleName}Result = ${moduleName}ResultRow

                      ${resultRowDecl moduleName resultRows.row}
                      ''
                  , Multiple =
                      ''
                      type ${moduleName}Result = Vector ${moduleName}ResultRow

                      ${resultRowDecl moduleName resultRows.row}
                      ''
                  }
                  resultRows.category
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
    : Input -> Output
    = \(input : Input) ->
        let projectNamespace = input.projectNamespace

        let moduleName = Name.toTextInPascal input.query.name

        let namespace = "${projectNamespace}.Statements.${moduleName}"

        in  { namespace
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

                ${Snippets.recordDataDecl
                    { name = moduleName
                    , fields =
                        Prelude.List.map
                          Model.Field
                          { name : Text, sig : Text }
                          ( \(field : Model.Field) ->
                              { name = Name.toTextInCamel field.name
                              , sig = valueSig field.value
                              }
                          )
                          input.query.params
                    }}
                  deriving stock (Eq, Show)

                ${resultDecls moduleName input.query.result}
                ''
            }

in  { Input, Output, compile }

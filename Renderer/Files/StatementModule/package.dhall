-- Template of a statement module.
--
-- Captures only what is needed for rendering it.
\(TODO : forall (a : Type) -> a) ->
  let Prelude = ../../../Prelude.dhall

  let Lude = ../../../Lude.dhall

  let NonEmpty = Prelude.NonEmpty.Type

  let Primitive =
        < Bool
        | Int2
        | Int4
        | Int8
        | Float4
        | Float8
        | Numeric
        | Text
        | Bytea
        | Date
        | Timestamp
        | Timestamptz
        | Time
        | Timetz
        | Interval
        | Uuid
        | Inet
        | Macaddr
        | Json
        | Jsonb
        | Hstore
        >

  let ParamElement = < Primitive : Primitive | Custom : Text >

  let ParamArray =
        { dimensionality : Natural
        , elementIsNullable : Bool
        , element : ParamElement
        }

  let ParamType = < Primitive | Custom : Text | Array : ParamArray >

  let Param = { haskellFieldName : Text, nullable : Bool, type_ : ParamType }

  let ResultCardinality = < ZeroOrOne | StrictlyOne | ZeroOrMore >

  let ResultPrimitive = (./ResultPrimitive.dhall).Input

  let ResultArrayElement = < Primitive : ResultPrimitive | Custom : Text >

  let ResultArray =
        { dimensionality : Natural
        , elementIsNullable : Bool
        , element : ResultArrayElement
        }

  let ResultColumnValue =
        < Primitive : ResultPrimitive | Array : ResultArray | Custom : Text >

  let ResultColumn =
        { haskellFieldName : Text
        , isNullable : Bool
        , decoder : ResultColumnValue
        }

  let ResultRow = NonEmpty ResultColumn

  let ResultRows = { cardinality : ResultCardinality, row : ResultRow }

  let Result = < None | RowsAffected | Rows : ResultRows >

  let Input =
        { projectNamespace : Text
        , baseName : Text
        , sql : Text
        , params : List Param
        , result : Result
        }

  let renderResultPrimitiveDecoder
      : ResultPrimitive -> Text
      = \(primitive : ResultPrimitive) ->
          merge
            { Bool = "Decoders.bool"
            , Int2 = "Decoders.int2"
            , Int4 = "Decoders.int4"
            , Int8 = "Decoders.int8"
            , Float4 = "Decoders.float4"
            , Float8 = "Decoders.float8"
            , Numeric = "Decoders.numeric"
            , Text = "Decoders.text"
            , Bytea = "Decoders.bytea"
            , Date = "Decoders.date"
            , Timestamp = "Decoders.timestamp"
            , Timestamptz = "Decoders.timestamptz"
            , Time = "Decoders.time"
            , Timetz = "Decoders.timetz"
            , Interval = "Decoders.interval"
            , Uuid = "Decoders.uuid"
            , Inet = "Decoders.inet"
            , Macaddr = "Decoders.macaddr"
            , Json = "Decoders.json"
            , Jsonb = "Decoders.jsonb"
            , Hstore = "Map.fromList <\$> Decoders.hstore replicateM"
            }
            primitive

  let renderResultRowDataDecl
      : Text -> ResultRow -> Text
      = let renderResultPrimitiveSig
            : ResultPrimitive -> Text
            = \(primitive : ResultPrimitive) ->
                merge
                  { Bool = "Bool"
                  , Int2 = "Int16"
                  , Int4 = "Int32"
                  , Int8 = "Int64"
                  , Float4 = "Float"
                  , Float8 = "Double"
                  , Numeric = "Scientific"
                  , Text = "Text"
                  , Bytea = "ByteString"
                  , Date = "Day"
                  , Timestamp = "LocalTime"
                  , Timestamptz = "UTCTime"
                  , Time = "TimeOfDay"
                  , Timetz = "(TimeOfDay, TimeZone)"
                  , Interval = "DiffTime"
                  , Uuid = "UUID"
                  , Inet = "IPRange"
                  , Macaddr = "(Word8, Word8, Word8, Word8, Word8, Word8)"
                  , Json = "Data.Aeson.Value"
                  , Jsonb = "Data.Aeson.Value"
                  , Hstore = "Map Text (Maybe Text)"
                  }
                  primitive

        let renderCustomTypeSig
            : Text -> Text
            = \(customType : Text) -> "CustomTypes." ++ customType

        let renderResultArrayElementSig
            : ResultArrayElement -> Text
            = \(element : ResultArrayElement) ->
                merge
                  { Primitive = renderResultPrimitiveSig
                  , Custom = renderCustomTypeSig
                  }
                  element

        let renderResultArraySig
            : ResultArray -> Text
            = \(input : ResultArray) ->
                let prefix =
                      Prelude.Text.replicate input.dimensionality "Vector ("

                let suffix = Prelude.Text.replicate input.dimensionality ")"

                let base =
                      if    input.elementIsNullable
                      then      "Maybe ("
                            ++  renderResultArrayElementSig input.element
                            ++  ")"
                      else  renderResultArrayElementSig input.element

                in  Prelude.Text.concat [ prefix, base, suffix ]

        let renderResultColumnValueSig
            : ResultColumnValue -> Text
            = \(value : ResultColumnValue) ->
                merge
                  { Primitive = renderResultPrimitiveSig
                  , Array = renderResultArraySig
                  , Custom = renderCustomTypeSig
                  }
                  value

        let renderResultColumnFieldDecl
            : ResultColumn -> Text
            = \(column : ResultColumn) ->
                let sig =
                      if    column.isNullable
                      then      "Maybe ("
                            ++  renderResultColumnValueSig column.decoder
                            ++  ")"
                      else  renderResultColumnValueSig column.decoder

                in  column.haskellFieldName ++ " :: " ++ sig

        in  \(baseName : Text) ->
            \(row : ResultRow) ->
              ''
              data ${baseName}ResultRow = ${baseName}ResultRow
                { ${Lude.Extensions.Text.indent
                      4
                      ( Prelude.Text.concatMapSep
                          ''
                          ,
                          ''
                          ResultColumn
                          renderResultColumnFieldDecl
                          (Prelude.NonEmpty.toList ResultColumn row)
                      )}
                }
                deriving stock (Eq, Show)''

  let renderResultDecls
      : Text -> Result -> Text
      = \(baseName : Text) ->
        \(result : Result) ->
          merge
            { None = "type ${baseName}Result = ()"
            , RowsAffected = "type ${baseName}Result = Int"
            , Rows =
                \(resultRows : ResultRows) ->
                  let resultRowDecl =
                        renderResultRowDataDecl baseName resultRows.row

                  let resultDecl =
                        merge
                          { ZeroOrOne =
                              "type ${baseName}Result = Maybe ${baseName}ResultRow"
                          , StrictlyOne =
                              "type ${baseName}Result = ${baseName}ResultRow"
                          , ZeroOrMore =
                              "type ${baseName}Result = Vector ${baseName}ResultRow"
                          }
                          resultRows.cardinality

                  in  ''
                      ${resultDecl}

                      ${resultRowDecl}''
            }
            result

  let render
      : Input -> Text
      = \(input : Input) -> TODO Text

  in  { render
      , Input
      , Param
      , Result
      , ResultRows
      , ResultCardinality
      , ResultColumn
      }

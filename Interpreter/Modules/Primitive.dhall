let Algebra = ../Algebra.dhall

let Sdk = Algebra.Sdk

let Lude = Algebra.Lude

let Model = Algebra.Model

let Input = Model.Primitive

let Output = { sig : Text, decoderName : Text }

let Result = Sdk.Compiled.Type Output

let Result/unsupportedType =
      \(type : Text) -> Sdk.Compiled.report Output [ type ] "Unsupported type"

let Result/ok =
      \(sig : Text) ->
      \(decoderName : Text) ->
        Sdk.Compiled.ok Output { sig, decoderName }

let run =
      \(input : Input) ->
        merge
          { Bool = Result/ok "Bool" "bool"
          , Bytea = Result/ok "ByteString" "bytea"
          , Char = Result/unsupportedType "char"
          , Cidr = Result/unsupportedType "cidr"
          , Date = Result/unsupportedType "date"
          , Datemultirange = Result/unsupportedType "datemultirange"
          , Daterange = Result/unsupportedType "daterange"
          , Float4 = Result/ok "Float" "float4"
          , Float8 = Result/ok "Double" "float8"
          , Inet = Result/unsupportedType "inet"
          , Int2 = Result/ok "Int16" "int2"
          , Int4 = Result/ok "Int32" "int4"
          , Int4multirange = Result/unsupportedType "int4multirange"
          , Int4range = Result/unsupportedType "int4range"
          , Int8 = Result/ok "Int64" "int8"
          , Int8multirange = Result/unsupportedType "int8multirange"
          , Int8range = Result/unsupportedType "int8range"
          , Interval = Result/ok "DiffTime" "interval"
          , Json = Result/unsupportedType "json"
          , Jsonb = Result/unsupportedType "jsonb"
          , Macaddr = Result/unsupportedType "macaddr"
          , Macaddr8 = Result/unsupportedType "macaddr8"
          , Money = Result/unsupportedType "money"
          , Numeric = Result/unsupportedType "numeric"
          , Nummultirange = Result/unsupportedType "nummultirange"
          , Numrange = Result/unsupportedType "numrange"
          , Text = Result/ok "Text" "text"
          , Time = Result/unsupportedType "time"
          , Timestamp = Result/ok "LocalTime" "timestamp"
          , Timestamptz = Result/ok "UTCTime" "timestamptz"
          , Timetz = Result/unsupportedType "timetz"
          , Tsmultirange = Result/unsupportedType "tsmultirange"
          , Tsrange = Result/unsupportedType "tsrange"
          , Tstzmultirange = Result/unsupportedType "tstzmultirange"
          , Tstzrange = Result/unsupportedType "tstzrange"
          , Uuid = Result/ok "Uuid" "uuid"
          , Xml = Result/unsupportedType "xml"
          }
          input

in  Algebra.module Input Output run

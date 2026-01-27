let Deps = ../Deps/package.dhall

let Algebra = ./Algebra/package.dhall

let Input = Deps.Sdk.Project.Primitive

let Output = { sig : Text, codecName : Text }

let unsupportedType =
      \(type : Text) ->
        Deps.Sdk.Compiled.report Output [ type ] "Unsupported type"

let ok =
      \(sig : Text) ->
      \(codecName : Text) ->
        Deps.Sdk.Compiled.ok Output { sig, codecName }

let run =
      \(config : Algebra.Config) ->
      \(input : Input) ->
        merge
          { Bool = ok "Bool" "bool"
          , Bytea = ok "ByteString" "bytea"
          , Char = unsupportedType "char"
          , Cidr = unsupportedType "cidr"
          , Date = unsupportedType "date"
          , Datemultirange = unsupportedType "datemultirange"
          , Daterange = unsupportedType "daterange"
          , Float4 = ok "Float" "float4"
          , Float8 = ok "Double" "float8"
          , Inet = unsupportedType "inet"
          , Int2 = ok "Int16" "int2"
          , Int4 = ok "Int32" "int4"
          , Int4multirange = unsupportedType "int4multirange"
          , Int4range = unsupportedType "int4range"
          , Int8 = ok "Int64" "int8"
          , Int8multirange = unsupportedType "int8multirange"
          , Int8range = unsupportedType "int8range"
          , Interval = ok "DiffTime" "interval"
          , Json = unsupportedType "json"
          , Jsonb = ok "Aeson.Value" "jsonb"
          , Macaddr = unsupportedType "macaddr"
          , Macaddr8 = unsupportedType "macaddr8"
          , Money = unsupportedType "money"
          , Numeric = unsupportedType "numeric"
          , Nummultirange = unsupportedType "nummultirange"
          , Numrange = unsupportedType "numrange"
          , Text = ok "Text" "text"
          , Time = unsupportedType "time"
          , Timestamp = ok "LocalTime" "timestamp"
          , Timestamptz = ok "UTCTime" "timestamptz"
          , Timetz = unsupportedType "timetz"
          , Tsmultirange = unsupportedType "tsmultirange"
          , Tsrange = unsupportedType "tsrange"
          , Tstzmultirange = unsupportedType "tstzmultirange"
          , Tstzrange = unsupportedType "tstzrange"
          , Uuid = ok "Uuid" "uuid"
          , Xml = unsupportedType "xml"
          }
          input

in  Algebra.module Input Output run

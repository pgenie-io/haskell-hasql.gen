let Algebra = ../Algebra.dhall

let Sdk = Algebra.Sdk

let Lude = Algebra.Lude

let Model = Algebra.Model

let Input = Model.Primitive

let Output = { sig : Text, decoderName : Text }

let Result = Lude.Structures.Result.Type Algebra.Error Output

let Result/unsupportedType =
      \(type : Text) ->
        Result.Failure { path = [ type ], message = "Unsupported type" }

let run =
      \(input : Input) ->
        merge
          { Bool = Result.Success { sig = "Bool", decoderName = "bool" }
          , Bytea = Result/unsupportedType "bytea"
          , Char = Result/unsupportedType "char"
          , Cidr = Result/unsupportedType "cidr"
          , Date = Result/unsupportedType "date"
          , Datemultirange = Result/unsupportedType "datemultirange"
          , Daterange = Result/unsupportedType "daterange"
          , Float4 = Result/unsupportedType "float4"
          , Float8 = Result/unsupportedType "float8"
          , Inet = Result/unsupportedType "inet"
          , Int2 = Result/unsupportedType "int2"
          , Int4 = Result/unsupportedType "int4"
          , Int4multirange = Result/unsupportedType "int4multirange"
          , Int4range = Result/unsupportedType "int4range"
          , Int8 = Result/unsupportedType "int8"
          , Int8multirange = Result/unsupportedType "int8multirange"
          , Int8range = Result/unsupportedType "int8range"
          , Interval = Result/unsupportedType "interval"
          , Json = Result/unsupportedType "json"
          , Jsonb = Result/unsupportedType "jsonb"
          , Macaddr = Result/unsupportedType "macaddr"
          , Macaddr8 = Result/unsupportedType "macaddr8"
          , Money = Result/unsupportedType "money"
          , Numeric = Result/unsupportedType "numeric"
          , Nummultirange = Result/unsupportedType "nummultirange"
          , Numrange = Result/unsupportedType "numrange"
          , Text = Result/unsupportedType "text"
          , Time = Result/unsupportedType "time"
          , Timestamp = Result/unsupportedType "timestamp"
          , Timestamptz = Result/unsupportedType "timestamptz"
          , Timetz = Result/unsupportedType "timetz"
          , Tsmultirange = Result/unsupportedType "tsmultirange"
          , Tsrange = Result/unsupportedType "tsrange"
          , Tstzmultirange = Result/unsupportedType "tstzmultirange"
          , Tstzrange = Result/unsupportedType "tstzrange"
          , Uuid = Result/unsupportedType "uuid"
          , Xml = Result/unsupportedType "xml"
          }
          input

in  Algebra.module Input Output run

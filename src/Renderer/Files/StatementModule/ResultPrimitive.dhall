let Prelude = ../../../Prelude.dhall

let Lude = ../../../Lude.dhall

let NonEmpty = Prelude.NonEmpty.Type

let Input =
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

let Output = { sig : Text, decoder : Text }

let render
    : Input -> Output
    = \(input : Input) ->
        merge
          { Bool = { sig = "Bool", decoder = "Decoders.bool" }
          , Int2 = { sig = "Int16", decoder = "Decoders.int2" }
          , Int4 = { sig = "Int32", decoder = "Decoders.int4" }
          , Int8 = { sig = "Int64", decoder = "Decoders.int8" }
          , Float4 = { sig = "Float", decoder = "Decoders.float4" }
          , Float8 = { sig = "Double", decoder = "Decoders.float8" }
          , Numeric = { sig = "Scientific", decoder = "Decoders.numeric" }
          , Text = { sig = "Text", decoder = "Decoders.text" }
          , Bytea = { sig = "ByteString", decoder = "Decoders.bytea" }
          , Date = { sig = "Day", decoder = "Decoders.date" }
          , Timestamp = { sig = "LocalTime", decoder = "Decoders.timestamp" }
          , Timestamptz = { sig = "UTCTime", decoder = "Decoders.timestamptz" }
          , Time = { sig = "TimeOfDay", decoder = "Decoders.time" }
          , Timetz =
            { sig = "(TimeOfDay, TimeZone)", decoder = "Decoders.timetz" }
          , Interval = { sig = "DiffTime", decoder = "Decoders.interval" }
          , Uuid = { sig = "UUID", decoder = "Decoders.uuid" }
          , Inet = { sig = "IPRange", decoder = "Decoders.inet" }
          , Macaddr =
            { sig = "(Word8, Word8, Word8, Word8, Word8, Word8)"
            , decoder = "Decoders.macaddr"
            }
          , Json = { sig = "Data.Aeson.Value", decoder = "Decoders.json" }
          , Jsonb = { sig = "Data.Aeson.Value", decoder = "Decoders.jsonb" }
          , Hstore =
            { sig = "Map Text (Maybe Text)"
            , decoder = "Map.fromList <\$> Decoders.hstore replicateM"
            }
          }
          input

in  { Input, Output, render }

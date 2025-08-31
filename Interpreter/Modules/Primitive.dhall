let Algebra = ../Algebra.dhall

let Sdk = Algebra.Sdk

let Lude = Algebra.Lude

let Model = Algebra.Model

let Input = Model.Primitive

let Output = { sig : Text, decoderName : Text }

let Error = < UnsupportedType >

let Result = Lude.Structures.Result.Type Error Output

let run =
      \(input : Input) ->
        merge
          { Bool = Result.Success { sig = "Bool", decoderName = "bool" }
          , Bytea = Result.Failure Error.UnsupportedType
          , Char = Result.Failure Error.UnsupportedType
          , Cidr = Result.Failure Error.UnsupportedType
          , Date = Result.Failure Error.UnsupportedType
          , Datemultirange = Result.Failure Error.UnsupportedType
          , Daterange = Result.Failure Error.UnsupportedType
          , Float4 = Result.Failure Error.UnsupportedType
          , Float8 = Result.Failure Error.UnsupportedType
          , Inet = Result.Failure Error.UnsupportedType
          , Int2 = Result.Failure Error.UnsupportedType
          , Int4 = Result.Failure Error.UnsupportedType
          , Int4multirange = Result.Failure Error.UnsupportedType
          , Int4range = Result.Failure Error.UnsupportedType
          , Int8 = Result.Failure Error.UnsupportedType
          , Int8multirange = Result.Failure Error.UnsupportedType
          , Int8range = Result.Failure Error.UnsupportedType
          , Interval = Result.Failure Error.UnsupportedType
          , Json = Result.Failure Error.UnsupportedType
          , Jsonb = Result.Failure Error.UnsupportedType
          , Macaddr = Result.Failure Error.UnsupportedType
          , Macaddr8 = Result.Failure Error.UnsupportedType
          , Money = Result.Failure Error.UnsupportedType
          , Numeric = Result.Failure Error.UnsupportedType
          , Nummultirange = Result.Failure Error.UnsupportedType
          , Numrange = Result.Failure Error.UnsupportedType
          , Text = Result.Failure Error.UnsupportedType
          , Time = Result.Failure Error.UnsupportedType
          , Timestamp = Result.Failure Error.UnsupportedType
          , Timestamptz = Result.Failure Error.UnsupportedType
          , Timetz = Result.Failure Error.UnsupportedType
          , Tsmultirange = Result.Failure Error.UnsupportedType
          , Tsrange = Result.Failure Error.UnsupportedType
          , Tstzmultirange = Result.Failure Error.UnsupportedType
          , Tstzrange = Result.Failure Error.UnsupportedType
          , Uuid = Result.Failure Error.UnsupportedType
          , Xml = Result.Failure Error.UnsupportedType
          }
          input

in  Algebra.module Input Output Error run

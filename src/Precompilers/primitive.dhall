let Prelude = ../Prelude.dhall

let Lude = ../Lude.dhall

let Sdk = ../Sdk.dhall

let Model = Sdk.Project

let Snippets = ../Snippets/package.dhall

let CodegenKit = ../CodegenKit.dhall

let Name = CodegenKit.Name

let Result = Lude.Structures.Result

let handler =
      { Bool =
        { sig = Some "Bool"
        , encoder = Some "Encoders.bool"
        , decoder = Some "Decoders.bool"
        }
      , Bytea = { sig = None Text, encoder = None Text, decoder = None Text }
      , Char = { sig = None Text, encoder = None Text, decoder = None Text }
      , Cidr = { sig = None Text, encoder = None Text, decoder = None Text }
      , Date = { sig = None Text, encoder = None Text, decoder = None Text }
      , Datemultirange =
        { sig = None Text, encoder = None Text, decoder = None Text }
      , Daterange =
        { sig = None Text, encoder = None Text, decoder = None Text }
      , Float4 = { sig = None Text, encoder = None Text, decoder = None Text }
      , Float8 = { sig = None Text, encoder = None Text, decoder = None Text }
      , Inet = { sig = None Text, encoder = None Text, decoder = None Text }
      , Int2 = { sig = None Text, encoder = None Text, decoder = None Text }
      , Int4 = { sig = None Text, encoder = None Text, decoder = None Text }
      , Int4multirange =
        { sig = None Text, encoder = None Text, decoder = None Text }
      , Int4range =
        { sig = None Text, encoder = None Text, decoder = None Text }
      , Int8 = { sig = None Text, encoder = None Text, decoder = None Text }
      , Int8multirange =
        { sig = None Text, encoder = None Text, decoder = None Text }
      , Int8range =
        { sig = None Text, encoder = None Text, decoder = None Text }
      , Interval = { sig = None Text, encoder = None Text, decoder = None Text }
      , Json = { sig = None Text, encoder = None Text, decoder = None Text }
      , Jsonb = { sig = None Text, encoder = None Text, decoder = None Text }
      , Macaddr = { sig = None Text, encoder = None Text, decoder = None Text }
      , Macaddr8 = { sig = None Text, encoder = None Text, decoder = None Text }
      , Money = { sig = None Text, encoder = None Text, decoder = None Text }
      , Numeric = { sig = None Text, encoder = None Text, decoder = None Text }
      , Nummultirange =
        { sig = None Text, encoder = None Text, decoder = None Text }
      , Numrange = { sig = None Text, encoder = None Text, decoder = None Text }
      , Text = { sig = None Text, encoder = None Text, decoder = None Text }
      , Time = { sig = None Text, encoder = None Text, decoder = None Text }
      , Timestamp =
        { sig = None Text, encoder = None Text, decoder = None Text }
      , Timestamptz =
        { sig = None Text, encoder = None Text, decoder = None Text }
      , Timetz = { sig = None Text, encoder = None Text, decoder = None Text }
      , Tsmultirange =
        { sig = None Text, encoder = None Text, decoder = None Text }
      , Tsrange = { sig = None Text, encoder = None Text, decoder = None Text }
      , Tstzmultirange =
        { sig = None Text, encoder = None Text, decoder = None Text }
      , Tstzrange =
        { sig = None Text, encoder = None Text, decoder = None Text }
      , Uuid = { sig = None Text, encoder = None Text, decoder = None Text }
      , Xml = { sig = None Text, encoder = None Text, decoder = None Text }
      }

in  \(model : Model.Primitive) ->
      merge
        handler
        model
      : { sig : Optional Text
        , encoder : Optional Text
        , decoder : Optional Text
        }

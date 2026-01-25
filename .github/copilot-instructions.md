We generate code for the Hasql v1.10.1 library for Haskell. Derive its API from [its docs](https://hackage-content.haskell.org/package/hasql-1.10.1).

# Loaded files

Assume the following contents for the according imports.

## `Sdk/Model`

The schema used for inputs to interpreter modules.

```dhall
let Prelude = ./Prelude.dhall

let CodegenKit = ./CodegenKit.dhall

let Version = { major : Natural, minor : Natural, patch : Natural }

let Name = CodegenKit.Name.Type

let Primitive =
      < Bool
      | Bytea
      | Char
      | Cidr
      | Date
      | Datemultirange
      | Daterange
      | Float4
      | Float8
      | Inet
      | Int2
      | Int4
      | Int4multirange
      | Int4range
      | Int8
      | Int8multirange
      | Int8range
      | Interval
      | Json
      | Jsonb
      | Macaddr
      | Macaddr8
      | Money
      | Numeric
      | Nummultirange
      | Numrange
      | Text
      | Time
      | Timestamp
      | Timestamptz
      | Timetz
      | Tsmultirange
      | Tsrange
      | Tstzmultirange
      | Tstzrange
      | Uuid
      | Xml
      >

let Scalar = < Primitive : Primitive | Custom : Name >

let ArraySettings = { dimensionality : Natural, elementIsNullable : Bool }

let Value = { arraySettings : Optional ArraySettings, scalar : Scalar }

let Member = { name : Name, pgName : Text, isNullable : Bool, value : Value }

let EnumVariant = { name : Name, pgName : Text }

let CustomTypeDefinition =
      < Composite : List Member | Enum : List EnumVariant | Domain : Value >

let CustomType =
      { name : Name, pgName : Text, definition : CustomTypeDefinition }

let ResultRowsCardinality = < Optional | Single | Multiple >

let ResultRows =
      { cardinality : ResultRowsCardinality, columns : Prelude.NonEmpty.Type Member }

let QueryFragment = < Sql : Text | Var : Name >

let Query =
      { name : Name
      , srcPath : Text
      , params : List Member
      , result : Optional ResultRows
      , fragments : List QueryFragment
      }

let Project =
      { name : Name
      , version : Version
      , customTypes : List CustomType
      , queries : List Query
      }

let Primitive/toText
    : Primitive -> Text
    = let handler =
            { Bool = "bool"
            , Bytea = "bytea"
            , Char = "char"
            , Cidr = "cidr"
            , Date = "date"
            , Datemultirange = "datemultirange"
            , Daterange = "daterange"
            , Float4 = "float4"
            , Float8 = "float8"
            , Inet = "inet"
            , Int2 = "int2"
            , Int4 = "int4"
            , Int4multirange = "int4multirange"
            , Int4range = "int4range"
            , Int8 = "int8"
            , Int8multirange = "int8multirange"
            , Int8range = "int8range"
            , Interval = "interval"
            , Json = "json"
            , Jsonb = "jsonb"
            , Macaddr = "macaddr"
            , Macaddr8 = "macaddr8"
            , Money = "money"
            , Numeric = "numeric"
            , Nummultirange = "nummultirange"
            , Numrange = "numrange"
            , Text = "text"
            , Time = "time"
            , Timestamp = "timestamp"
            , Timestamptz = "timestamptz"
            , Timetz = "timetz"
            , Tsmultirange = "tsmultirange"
            , Tsrange = "tsrange"
            , Tstzmultirange = "tstzmultirange"
            , Tstzrange = "tstzrange"
            , Uuid = "uuid"
            , Xml = "xml"
            }

      in  \(primitive : Primitive) -> merge handler primitive

in  { Project
    , Version
    , Name
    , Primitive
    , Primitive/toText
    , Scalar
    , ArraySettings
    , Value
    , EnumVariant
    , CustomTypeDefinition
    , CustomType
    , Member
    , ResultRowsCardinality
    , ResultRows
    , QueryFragment
    , Query
    }
```


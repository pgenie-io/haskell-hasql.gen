# haskell-hasql.gen

A [pGenie](https://github.com/pgenie-io/pgenie) plugin that generates type-safe Haskell bindings for PostgreSQL using the [Hasql](https://hackage.haskell.org/package/hasql) library.

## What it generates

For each pGenie project the plugin produces a self-contained Haskell package containing:

- **A `.cabal` file** – a ready-to-build library with all required dependencies declared.
- **`<Namespace>.Statements`** – one module per SQL query. Each module contains:
  - A parameter record type (e.g. `InsertAlbum`) with a field per query parameter.
  - A result type alias (e.g. `InsertAlbumResult`) with a corresponding row type.
  - An `IsStatement` instance that holds the compiled `Statement` value, including the SQL text, encoder, and decoder.
- **`<Namespace>.Types`** – one module per custom PostgreSQL type. Each module contains a Haskell type and its `IsScalar` instance for encoding/decoding:
  - **Enums** → `data` declarations with pattern-matched codec.
  - **Composite types** → record declarations with composite codec.
  - **Domain types** → type aliases over their base scalar.
- **`<Namespace>.Prelude`** – a re-export module bundling all standard imports needed by generated code.

All statements are compatible with the `hasql-mapping` execution API and can also be used directly via `hasql`.

## Using the plugin in a pGenie project

Add the plugin to your pGenie project configuration:

```dhall
{ plugins = [ "https://github.com/pgenie-io/haskell-hasql.gen" ]
, ...
}
```

Run the code generator:

```bash
pgenie generate
```

The generated package is placed in the output directory configured in your project. Add it to your Haskell project's `cabal.project` or `stack.yaml` as a local package.

## Supported PostgreSQL types

Scalar types can appear as plain values, as nullable values (`Maybe a`), or as arrays of any dimensionality (`Vector a`, `Vector (Vector a)`, …).

| PostgreSQL type | Haskell type              |
|-----------------|---------------------------|
| `bool`          | `Bool`                    |
| `bytea`         | `ByteString`              |
| `char`          | `Char`                    |
| `date`          | `Day`                     |
| `float4`        | `Float`                   |
| `float8`        | `Double`                  |
| `int2`          | `Int16`                   |
| `int4`          | `Int32`                   |
| `int8`          | `Int64`                   |
| `interval`      | `DiffTime`                |
| `jsonb`         | `Aeson.Value`             |
| `numeric`       | `Scientific`              |
| `text`          | `Text`                    |
| `timestamp`     | `LocalTime`               |
| `timestamptz`   | `UTCTime`                 |
| `timetz`        | `(TimeOfDay, TimeZone)`   |
| `uuid`          | `UUID`                    |

User-defined **enum**, **composite**, and **domain** types are also supported and generate corresponding Haskell types with `IsScalar` instances.

## Integrating generated code into a Haskell project

Add the generated package to your `cabal.project`:

```cabal
packages:
  .
  generated/my-space-music-catalogue
```

Declare the dependency in your own `.cabal` file:

```cabal
build-depends:
  my-space-music-catalogue,
  hasql,
```

Execute statements via `Hasql.Session`:

```haskell
import MySpace.MusicCatalogue.Statements
import qualified Hasql.Mapping.IsStatement as IsStatement
import qualified Hasql.Session as Session
import qualified Hasql.Connection as Connection

run :: Connection.Connection -> IO ()
run conn = do
  -- Execute a query that returns multiple rows
  result <- Session.run selectSession conn
  case result of
    Left err  -> putStrLn ("Query failed: " <> show err)
    Right rows -> mapM_ print rows

  -- Execute an insert that returns the new row id
  result2 <- Session.run insertSession conn
  case result2 of
    Left err -> putStrLn ("Insert failed: " <> show err)
    Right InsertAlbumResultRow { id } -> putStrLn ("Inserted id: " <> show id)

selectSession :: Session.Session (Vector SelectAlbumByNameResultRow)
selectSession =
  Session.statement
    SelectAlbumByName { name = Just "Rumours" }
    IsStatement.statement

insertSession :: Session.Session InsertAlbumResultRow
insertSession =
  Session.statement
    InsertAlbum
      { name      = "Rumours"
      , released  = read "1977-02-04"
      , format    = VinylAlbumFormat
      , recording = RecordingInfo
          { studioName   = Just "Sound Factory"
          , city         = Just "Los Angeles"
          , country      = Just "US"
          , recordedDate = Nothing
          }
      }
    IsStatement.statement
```

`Hasql.Mapping.IsStatement.statement` holds the underlying `Hasql.Statement.Statement` value built from the SQL template, encoder, and decoder generated for each query.

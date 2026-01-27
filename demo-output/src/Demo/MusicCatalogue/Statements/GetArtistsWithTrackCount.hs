module Demo.MusicCatalogue.Statements.GetArtistsWithTrackCount where

import qualified Hasql.Statement as Statement
import qualified Hasql.Decoders as Decoders
import qualified Hasql.Encoders as Encoders
import qualified Data.ByteString as ByteString
import qualified Data.Int as Int
import qualified Data.Text as Text
import qualified Data.Vector as Vector

-- |
-- Parameters for the @get_artists_with_track_count@ query.
--
-- == SQL Template
--
-- > SELECT 
-- >     ar.id,
-- >     ar.name,
-- >     COUNT(DISTINCT t.id) as track_count,
-- >     COUNT(DISTINCT a.id) as album_count
-- > FROM artists ar
-- > LEFT JOIN albums a ON ar.id = a.artist_id
-- > LEFT JOIN tracks t ON a.id = t.album_id
-- > GROUP BY ar.id, ar.name
-- > ORDER BY track_count DESC
--
-- == Source Path
--
-- > queries/get_artists_with_track_count.sql
--
data GetArtistsWithTrackCount = GetArtistsWithTrackCount
  { 
  }
  deriving stock (Eq, Show)


type GetArtistsWithTrackCountResult = Vector.Vector GetArtistsWithTrackCountResultRow

data GetArtistsWithTrackCountResultRow = GetArtistsWithTrackCountResultRow
  { id :: Uuid,
    name :: Text,
    trackCount :: Int32,
    albumCount :: Int32
  }

instance IsStatement GetArtistsWithTrackCount where
  type ResultOf GetArtistsWithTrackCount = GetArtistsWithTrackCountResult

  statementOf = Statement.prepared sql encoder decoder
    where
      sql =
        "SELECT \n\
        \    ar.id,\n\
        \    ar.name,\n\
        \    COUNT(DISTINCT t.id) as track_count,\n\
        \    COUNT(DISTINCT a.id) as album_count\n\
        \FROM artists ar\n\
        \LEFT JOIN albums a ON ar.id = a.artist_id\n\
        \LEFT JOIN tracks t ON a.id = t.album_id\n\
        \GROUP BY ar.id, ar.name\n\
        \ORDER BY track_count DESC"

      encoder =
        mconcat
          [ 
          ]

      decoder =
        Decoders.rowVector do
          id <- Decoders.column (Decoders.nonNullable (scalarDecoder))
          name <- Decoders.column (Decoders.nonNullable (scalarDecoder))
          trackCount <- Decoders.column (Decoders.nonNullable (scalarDecoder))
          albumCount <- Decoders.column (Decoders.nonNullable (scalarDecoder))
          pure GetArtistsWithTrackCountResultRow {..}


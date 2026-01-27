module Demo.MusicCatalogue.Statements.GetTopTracksByPlayCount where

import qualified Hasql.Statement as Statement
import qualified Hasql.Decoders as Decoders
import qualified Hasql.Encoders as Encoders
import qualified Data.ByteString as ByteString
import qualified Data.Int as Int
import qualified Data.Text as Text
import qualified Data.Vector as Vector

-- |
-- Parameters for the @get_top_tracks_by_play_count@ query.
--
-- == SQL Template
--
-- > SELECT 
-- >     t.id,
-- >     t.title,
-- >     ar.name as artist_name,
-- >     a.title as album_title,
-- >     COALESCE(p.play_count, 0) as play_count
-- > FROM tracks t
-- > JOIN albums a ON t.album_id = a.id
-- > JOIN artists ar ON a.artist_id = ar.id
-- > LEFT JOIN (
-- >     SELECT track_id, COUNT(*) as play_count
-- >     FROM play_history
-- >     GROUP BY track_id
-- > ) p ON t.id = p.track_id
-- > ORDER BY play_count DESC
-- > LIMIT $limit
--
-- == Source Path
--
-- > queries/get_top_tracks_by_play_count.sql
--
data GetTopTracksByPlayCount = GetTopTracksByPlayCount
  { limit :: Int32
  }
  deriving stock (Eq, Show)


type GetTopTracksByPlayCountResult = Vector.Vector GetTopTracksByPlayCountResultRow

data GetTopTracksByPlayCountResultRow = GetTopTracksByPlayCountResultRow
  { id :: Uuid,
    title :: Text,
    artistName :: Text,
    albumTitle :: Text,
    playCount :: Int32
  }

instance IsStatement GetTopTracksByPlayCount where
  type ResultOf GetTopTracksByPlayCount = GetTopTracksByPlayCountResult

  statementOf = Statement.prepared sql encoder decoder
    where
      sql =
        "SELECT \n\
        \    t.id,\n\
        \    t.title,\n\
        \    ar.name as artist_name,\n\
        \    a.title as album_title,\n\
        \    COALESCE(p.play_count, 0) as play_count\n\
        \FROM tracks t\n\
        \JOIN albums a ON t.album_id = a.id\n\
        \JOIN artists ar ON a.artist_id = ar.id\n\
        \LEFT JOIN (\n\
        \    SELECT track_id, COUNT(*) as play_count\n\
        \    FROM play_history\n\
        \    GROUP BY track_id\n\
        \) p ON t.id = p.track_id\n\
        \ORDER BY play_count DESC\n\
        \LIMIT $1"

      encoder =
        mconcat
          [ Encoders.param ((.limit) >$< Encoders.nonNullable (scalarEncoder))
          ]

      decoder =
        Decoders.rowVector do
          id <- Decoders.column (Decoders.nonNullable (scalarDecoder))
          title <- Decoders.column (Decoders.nonNullable (scalarDecoder))
          artistName <- Decoders.column (Decoders.nonNullable (scalarDecoder))
          albumTitle <- Decoders.column (Decoders.nonNullable (scalarDecoder))
          playCount <- Decoders.column (Decoders.nonNullable (scalarDecoder))
          pure GetTopTracksByPlayCountResultRow {..}


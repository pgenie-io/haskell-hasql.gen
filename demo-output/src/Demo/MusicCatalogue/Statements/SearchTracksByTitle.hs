module Demo.MusicCatalogue.Statements.SearchTracksByTitle where

import Demo.MusicCatalogue.Prelude
import qualified Hasql.Statement as Statement
import qualified Hasql.Decoders as Decoders
import qualified Hasql.Encoders as Encoders
import qualified Data.ByteString as ByteString
import qualified Data.Int as Int
import qualified Data.Text as Text
import qualified Data.Vector as Vector

-- |
-- Parameters for the @search_tracks_by_title@ query.
--
-- == SQL Template
--
-- > SELECT 
-- >     t.id,
-- >     t.title,
-- >     t.duration_seconds,
-- >     a.title as album_title,
-- >     ar.name as artist_name
-- > FROM tracks t
-- > JOIN albums a ON t.album_id = a.id
-- > JOIN artists ar ON a.artist_id = ar.id
-- > WHERE t.title ILIKE '%' || $search_term || '%' ORDER BY ar.name, a.title, t.track_number
--
-- == Source Path
--
-- > queries/search_tracks_by_title.sql
--
data SearchTracksByTitle = SearchTracksByTitle
  { searchTerm :: Text
  }
  deriving stock (Eq, Show)


type SearchTracksByTitleResult = Vector.Vector SearchTracksByTitleResultRow

data SearchTracksByTitleResultRow = SearchTracksByTitleResultRow
  { id :: UUID,
    title :: Text,
    duration :: Maybe (Int32),
    albumTitle :: Text,
    artistName :: Text
  }

instance IsStatement SearchTracksByTitle where
  type Result SearchTracksByTitle = SearchTracksByTitleResult

  statement = Statement.prepared sql encoder decoder
    where
      sql =
        "SELECT \n\
        \    t.id,\n\
        \    t.title,\n\
        \    t.duration_seconds,\n\
        \    a.title as album_title,\n\
        \    ar.name as artist_name\n\
        \FROM tracks t\n\
        \JOIN albums a ON t.album_id = a.id\n\
        \JOIN artists ar ON a.artist_id = ar.id\n\
        \WHERE t.title ILIKE '%' || $1 || '%' ORDER BY ar.name, a.title, t.track_number"

      encoder =
        mconcat
          [ Encoders.param ((.searchTerm) >$< Encoders.nonNullable (scalarEncoder))
          ]

      decoder =
        Decoders.rowVector do
          id <- Decoders.column (Decoders.nonNullable (scalarDecoder))
          title <- Decoders.column (Decoders.nonNullable (scalarDecoder))
          duration <- Decoders.column (Decoders.nullable (scalarDecoder))
          albumTitle <- Decoders.column (Decoders.nonNullable (scalarDecoder))
          artistName <- Decoders.column (Decoders.nonNullable (scalarDecoder))
          pure SearchTracksByTitleResultRow {..}


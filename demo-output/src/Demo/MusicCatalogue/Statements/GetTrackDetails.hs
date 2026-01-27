module Demo.MusicCatalogue.Statements.GetTrackDetails where

import Demo.MusicCatalogue.Prelude
import qualified Hasql.Statement as Statement
import qualified Hasql.Decoders as Decoders
import qualified Hasql.Encoders as Encoders
import qualified Data.ByteString as ByteString
import qualified Data.Int as Int
import qualified Data.Text as Text
import qualified Data.Vector as Vector

-- |
-- Parameters for the @get_track_details@ query.
--
-- == SQL Template
--
-- > SELECT 
-- >     t.id,
-- >     t.title,
-- >     t.duration_seconds,
-- >     t.track_number,
-- >     a.id as album_id,
-- >     a.title as album_title,
-- >     ar.id as artist_id,
-- >     ar.name as artist_name,
-- >     g.name as genre
-- > FROM tracks t
-- > JOIN albums a ON t.album_id = a.id
-- > JOIN artists ar ON a.artist_id = ar.id
-- > LEFT JOIN genres g ON t.genre_id = g.id
-- > WHERE t.id = $track_id
--
-- == Source Path
--
-- > queries/get_track_details.sql
--
data GetTrackDetails = GetTrackDetails
  { trackId :: UUID
  }
  deriving stock (Eq, Show)


type GetTrackDetailsResult = GetTrackDetailsResultRow

data GetTrackDetailsResultRow = GetTrackDetailsResultRow
  { id :: UUID,
    title :: Text,
    duration :: Maybe (Int32),
    trackNumber :: Maybe (Int32),
    albumId :: UUID,
    albumTitle :: Text,
    artistId :: UUID,
    artistName :: Text,
    genre :: Maybe (Text)
  }

instance IsStatement GetTrackDetails where
  type Result GetTrackDetails = GetTrackDetailsResult

  statement = Statement.prepared sql encoder decoder
    where
      sql =
        "SELECT \n\
        \    t.id,\n\
        \    t.title,\n\
        \    t.duration_seconds,\n\
        \    t.track_number,\n\
        \    a.id as album_id,\n\
        \    a.title as album_title,\n\
        \    ar.id as artist_id,\n\
        \    ar.name as artist_name,\n\
        \    g.name as genre\n\
        \FROM tracks t\n\
        \JOIN albums a ON t.album_id = a.id\n\
        \JOIN artists ar ON a.artist_id = ar.id\n\
        \LEFT JOIN genres g ON t.genre_id = g.id\n\
        \WHERE t.id = $1"

      encoder =
        mconcat
          [ Encoders.param ((.trackId) >$< Encoders.nonNullable (scalarEncoder))
          ]

      decoder =
        Decoders.singleRow do
          id <- Decoders.column (Decoders.nonNullable (scalarDecoder))
          title <- Decoders.column (Decoders.nonNullable (scalarDecoder))
          duration <- Decoders.column (Decoders.nullable (scalarDecoder))
          trackNumber <- Decoders.column (Decoders.nullable (scalarDecoder))
          albumId <- Decoders.column (Decoders.nonNullable (scalarDecoder))
          albumTitle <- Decoders.column (Decoders.nonNullable (scalarDecoder))
          artistId <- Decoders.column (Decoders.nonNullable (scalarDecoder))
          artistName <- Decoders.column (Decoders.nonNullable (scalarDecoder))
          genre <- Decoders.column (Decoders.nullable (scalarDecoder))
          pure GetTrackDetailsResultRow {..}


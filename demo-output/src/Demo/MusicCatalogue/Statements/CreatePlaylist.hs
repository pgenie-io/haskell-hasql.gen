module Demo.MusicCatalogue.Statements.CreatePlaylist where

import qualified Hasql.Statement as Statement
import qualified Hasql.Decoders as Decoders
import qualified Hasql.Encoders as Encoders
import qualified Data.ByteString as ByteString
import qualified Data.Int as Int
import qualified Data.Text as Text
import qualified Data.Vector as Vector

-- |
-- Parameters for the @create_playlist@ query.
--
-- == SQL Template
--
-- > INSERT INTO playlists (name, description, user_id, created_at)
-- > VALUES ($name, $description, $user_id, NOW())
-- > RETURNING id, name, created_at
--
-- == Source Path
--
-- > queries/create_playlist.sql
--
data CreatePlaylist = CreatePlaylist
  { name :: Text,
    description :: Maybe (Text),
    userId :: Uuid
  }
  deriving stock (Eq, Show)


type CreatePlaylistResult = CreatePlaylistResultRow

data CreatePlaylistResultRow = CreatePlaylistResultRow
  { id :: Uuid,
    name :: Text,
    createdAt :: LocalTime
  }

instance IsStatement CreatePlaylist where
  type ResultOf CreatePlaylist = CreatePlaylistResult

  statementOf = Statement.prepared sql encoder decoder
    where
      sql =
        "INSERT INTO playlists (name, description, user_id, created_at)\n\
        \VALUES ($2, $3, $1, NOW())\n\
        \RETURNING id, name, created_at"

      encoder =
        mconcat
          [ Encoders.param ((.name) >$< Encoders.nonNullable (scalarEncoder)),
            Encoders.param ((.description) >$< Encoders.nullable (scalarEncoder)),
            Encoders.param ((.userId) >$< Encoders.nonNullable (scalarEncoder))
          ]

      decoder =
        Decoders.singleRow do
          id <- Decoders.column (Decoders.nonNullable (scalarDecoder))
          name <- Decoders.column (Decoders.nonNullable (scalarDecoder))
          createdAt <- Decoders.column (Decoders.nonNullable (scalarDecoder))
          pure CreatePlaylistResultRow {..}


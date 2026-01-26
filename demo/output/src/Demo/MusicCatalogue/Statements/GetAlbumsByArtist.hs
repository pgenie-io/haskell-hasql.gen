module Demo.MusicCatalogue.Statements.GetAlbumsByArtist where

import qualified Hasql.Statement as Statement
import qualified Hasql.Decoders as Decoders
import qualified Hasql.Encoders as Encoders
import qualified Data.ByteString as ByteString
import qualified Data.Int as Int
import qualified Data.Text as Text
import qualified Data.Vector as Vector

-- |
-- Parameters for the @get_albums_by_artist@ query.
--
-- == SQL Template
--
-- > SELECT 
-- >     a.id,
-- >     a.title,
-- >     a.release_year,
-- >     a.album_type
-- > FROM albums a
-- > WHERE a.artist_id = $artist_id ORDER BY a.release_year DESC
--
-- == Source Path
--
-- > queries/get_albums_by_artist.sql
--
data GetAlbumsByArtist = GetAlbumsByArtist
  { artistId :: Uuid
  }
  deriving stock (Eq, Show)


type GetAlbumsByArtistResult = Vector.Vector GetAlbumsByArtistResultRow

data GetAlbumsByArtistResultRow = GetAlbumsByArtistResultRow
  { id :: Uuid,
    title :: Text,
    releaseYear :: Maybe (Int32),
    albumType :: CustomTypes.AlbumType
  }

instance IsStatement GetAlbumsByArtist where
  type ResultOf GetAlbumsByArtist = GetAlbumsByArtistResult

  statementOf = Statement.prepared sql encoder decoder
    where
      sql =
        "SELECT \n\
        \    a.id,\n\
        \    a.title,\n\
        \    a.release_year,\n\
        \    a.album_type\n\
        \FROM albums a\n\
        \WHERE a.artist_id = $1 ORDER BY a.release_year DESC"

      encoder =
        mconcat
          [ Encoders.param ((.artistId) >$< Encoders.nonNullable (valueEncoder))
          ]

      decoder =
        Decoders.rowVector do
          id <- Decoders.column ((.id) <$> Decoders.nonNullable (valueDecoder))
          title <- Decoders.column ((.title) <$> Decoders.nonNullable (valueDecoder))
          releaseYear <- Decoders.column ((.releaseYear) <$> Decoders.nullable (valueDecoder))
          albumType <- Decoders.column ((.albumType) <$> Decoders.nonNullable (valueDecoder))
          pure GetAlbumsByArtistResultRow {..}


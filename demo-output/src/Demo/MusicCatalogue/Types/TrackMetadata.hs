module Demo.MusicCatalogue.Types.TrackMetadata where

import Demo.MusicCatalogue.Prelude
import qualified Data.Aeson as Aeson
import qualified Data.Vector as Vector
import qualified Hasql.Decoders as Decoders
import qualified Hasql.Encoders as Encoders
import qualified Hasql.Mapping.IsScalar as IsScalar

-- |
-- Representation of the @track_metadata@ user-declared PostgreSQL record type.
data TrackMetadata = TrackMetadata
  { -- | Maps to @title@.
    title :: Text,
    -- | Maps to @metadata@.
    metadata :: Maybe (Vector (Maybe Aeson.Value)),
    -- | Maps to @created_at@.
    createdAt :: LocalTime
  }
  deriving stock (Show, Eq, Ord)

instance IsScalar.IsScalar TrackMetadata where
  encoder =
    Encoders.composite
      (Just "music_catalogue")
      "track_metadata"
      ( mconcat
          [ (.title) >$< Encoders.field (Encoders.nonNullable (IsScalar.encoder)),
            (.metadata) >$< Encoders.field (Encoders.nullable (Encoders.array (Encoders.dimension Vector.foldl' (Encoders.element (Encoders.nullable IsScalar.encoder))))),
            (.createdAt) >$< Encoders.field (Encoders.nonNullable (IsScalar.encoder))
          ]
      )
  
  decoder =
    Decoders.composite
      (Just "music_catalogue")
      "track_metadata"
      ( TrackMetadata
          <$> Decoders.field (Decoders.nonNullable (IsScalar.decoder))
          <*> Decoders.field (Decoders.nullable (Decoders.array (Decoders.dimension Vector.replicateM (Decoders.element (Decoders.nullable IsScalar.decoder)))))
          <*> Decoders.field (Decoders.nonNullable (IsScalar.decoder))
      )
  

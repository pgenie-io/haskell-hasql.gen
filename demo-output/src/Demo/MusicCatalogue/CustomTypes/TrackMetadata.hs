module Demo.MusicCatalogue.CustomTypes.TrackMetadata where

import Demo.MusicCatalogue.Prelude
import Hasql.Mapping.Scalar

-- |
-- Representation of the @track_metadata@ user-declared PostgreSQL record type.
data TrackMetadata = TrackMetadata
  { title :: Text,
    metadata :: Maybe (Vector (Maybe Aeson.Value)),
    createdAt :: LocalTime
  }
  deriving stock (Show, Eq, Ord)

instance IsScalar TrackMetadata where
  scalarEncoder =
    Encoders.composite
      (Just "music_catalogue")
      "track_metadata"
      ( mconcat
          [ Encoders.field ((.title) >$< Encoders.nonNullable (scalarEncoder)),
            Encoders.field ((.metadata) >$< Encoders.nullable (Encoders.array (Encoders.dimension Vector.foldl' (Encoders.element (Encoders.nullable scalarEncoder))))),
            Encoders.field ((.createdAt) >$< Encoders.nonNullable (scalarEncoder))
          ]
      )
  
  scalarDecoder =
    Decoders.composite
      (Just "music_catalogue")
      "track_metadata"
      ( TrackMetadata
          <$> Decoders.field (Decoders.nonNullable (scalarDecoder))
          <*> Decoders.field (Decoders.nullable (Decoders.array (Decoders.dimension Vector.replicateM (Decoders.element (Decoders.nullable scalarDecoder)))))
          <*> Decoders.field (Decoders.nonNullable (scalarDecoder))
      )
  

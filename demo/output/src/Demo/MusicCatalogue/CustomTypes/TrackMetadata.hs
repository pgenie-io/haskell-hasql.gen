module Demo.MusicCatalogue.CustomTypes.TrackMetadata where

import Demo.MusicCatalogue.Preludes.CustomType
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
  valueEncoder :: Encoders.Value TrackMetadata
  valueEncoder =
    Encoders.composite
      (Just "music_catalogue")
      "track_metadata"
      ( mconcat
          [ Encoders.field ((.title) >$< Encoders.nonNullable (valueEncoder)),
            Encoders.field ((.metadata) >$< Encoders.nullable (Encoders.array (Encoders.dimension Vector.foldl' (Encoders.element (Encoders.nullable valueEncoder))))),
            Encoders.field ((.createdAt) >$< Encoders.nonNullable (valueEncoder))
          ]
      )
  
  valueDecoder :: Decoders.Value TrackMetadata
  valueDecoder =
    Decoders.composite
      (Just "music_catalogue")
      "track_metadata"
      ( TrackMetadata
          <$> Decoders.field ((.title) <$> Decoders.nonNullable (valueDecoder))
          <*> Decoders.field ((.metadata) <$> Decoders.nullable (Decoders.array (Decoders.dimension Vector.replicateM (Decoders.element (Decoders.nullable valueDecoder)))))
          <*> Decoders.field ((.createdAt) <$> Decoders.nonNullable (valueDecoder))
      )
  

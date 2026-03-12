module Demo.MusicCatalogue.Types.AlbumType where

import Demo.MusicCatalogue.Prelude
import qualified Hasql.Decoders as Decoders
import qualified Hasql.Encoders as Encoders
import qualified Hasql.Mapping.IsScalar as IsScalar

-- |
-- Representation of the @album_type@ user-declared PostgreSQL enumeration type.
data AlbumType
  = -- | Corresponds to the PostgreSQL enum variant @studio@.
    StudioAlbumType
  | -- | Corresponds to the PostgreSQL enum variant @live@.
    LiveAlbumType
  | -- | Corresponds to the PostgreSQL enum variant @compilation@.
    CompilationAlbumType
  | -- | Corresponds to the PostgreSQL enum variant @soundtrack@.
    SoundtrackAlbumType
  | -- | Corresponds to the PostgreSQL enum variant @ep@.
    EpAlbumType
  | -- | Corresponds to the PostgreSQL enum variant @single@.
    SingleAlbumType
  deriving stock (Show, Eq, Ord, Enum, Bounded)

instance IsScalar.IsScalar AlbumType where
  encoder =
    Encoders.enum
      (Just "music_catalogue")
      "album_type"
      ( \case
          StudioAlbumType -> "studio"
          LiveAlbumType -> "live"
          CompilationAlbumType -> "compilation"
          SoundtrackAlbumType -> "soundtrack"
          EpAlbumType -> "ep"
          SingleAlbumType -> "single"
      )
  
  decoder =
    Decoders.enum
      (Just "music_catalogue")
      "album_type"
      ( \case
          "studio" -> Just StudioAlbumType
          "live" -> Just LiveAlbumType
          "compilation" -> Just CompilationAlbumType
          "soundtrack" -> Just SoundtrackAlbumType
          "ep" -> Just EpAlbumType
          "single" -> Just SingleAlbumType
          _ -> Nothing
      )

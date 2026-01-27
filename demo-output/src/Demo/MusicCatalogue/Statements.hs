-- | Mappings to all queries in the project.
-- 
-- Hasql statements are provided by the 'Hasql.Mapping.IsStatement' typeclass instances indexed by the statement parameter type.
-- 
module Demo.MusicCatalogue.Statements 
  ( module Demo.MusicCatalogue.Statements.GetAlbumsByArtist,
    module Demo.MusicCatalogue.Statements.SearchTracksByTitle,
    module Demo.MusicCatalogue.Statements.GetTrackDetails,
    module Demo.MusicCatalogue.Statements.GetArtistsWithTrackCount,
    module Demo.MusicCatalogue.Statements.CreatePlaylist,
    module Demo.MusicCatalogue.Statements.GetTopTracksByPlayCount,
  )
where

import Demo.MusicCatalogue.Statements.GetAlbumsByArtist
import Demo.MusicCatalogue.Statements.SearchTracksByTitle
import Demo.MusicCatalogue.Statements.GetTrackDetails
import Demo.MusicCatalogue.Statements.GetArtistsWithTrackCount
import Demo.MusicCatalogue.Statements.CreatePlaylist
import Demo.MusicCatalogue.Statements.GetTopTracksByPlayCount

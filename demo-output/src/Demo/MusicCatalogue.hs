module Demo.MusicCatalogue
  ( -- * Execution
    
    -- ** Levels
    Hasql.Pipeline,
    Hasql.Session,
    Hasql.Transaction,
    
    -- ** Execution functions
    runInPipeline,
    runInSession,
    runInTransaction,
    
    -- * Statement Typeclass
    IsStatement,
    
    -- * Statements
    -- ** GetAlbumsByArtist statement
    module Demo.MusicCatalogue.Statements.GetAlbumsByArtist,
    -- ** SearchTracksByTitle statement
    module Demo.MusicCatalogue.Statements.SearchTracksByTitle,
    -- ** GetTrackDetails statement
    module Demo.MusicCatalogue.Statements.GetTrackDetails,
    -- ** GetArtistsWithTrackCount statement
    module Demo.MusicCatalogue.Statements.GetArtistsWithTrackCount,
    -- ** CreatePlaylist statement
    module Demo.MusicCatalogue.Statements.CreatePlaylist,
    -- ** GetTopTracksByPlayCount statement
    module Demo.MusicCatalogue.Statements.GetTopTracksByPlayCount
  )
where

import Demo.MusicCatalogue.Statements.GetAlbumsByArtist
import Demo.MusicCatalogue.Statements.SearchTracksByTitle
import Demo.MusicCatalogue.Statements.GetTrackDetails
import Demo.MusicCatalogue.Statements.GetArtistsWithTrackCount
import Demo.MusicCatalogue.Statements.CreatePlaylist
import Demo.MusicCatalogue.Statements.GetTopTracksByPlayCount

runInPipeline :: (IsStatement a) => a -> Hasql.Pipeline b


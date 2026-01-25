module Demo.MusicCatalogue.Algebra.Statement where

import Prelude
import Data.Text (Text)
import Data.Functor.Contravariant ((>$<))
import qualified Hasql.Encoders as Encoders
import qualified Hasql.Decoders as Decoders
import Data.Int (Int64, Int32, Int16)
import Data.Scientific (Scientific)
import Data.UUID (UUID)

class IsStatement a where
  type ResultOf a
  statementOf :: Statement.Statement a (ResultOf a)
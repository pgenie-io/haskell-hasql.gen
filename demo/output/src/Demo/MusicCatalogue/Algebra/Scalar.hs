module Demo.MusicCatalogue.Algebras.Scalar where

import Prelude
import Data.Text (Text)
import Data.Functor.Contravariant ((>$<))
import qualified Hasql.Encoders as Encoders
import qualified Hasql.Decoders as Decoders
import Data.Int (Int64, Int32, Int16)
import Data.Scientific (Scientific)
import Data.UUID (UUID)

class IsScalar a where
  encoder :: Encoders.Value a
  decoder :: Decoders.Value a

instance IsScalar Int where
  encoder = fromIntegral >$< Encoders.int8
  decoder = fromIntegral <$> Decoders.int8

instance IsScalar Int64 where
  encoder = Encoders.int8
  decoder = Decoders.int8

instance IsScalar Bool where
  encoder = Encoders.bool
  decoder = Decoders.bool

instance IsScalar Text where
  encoder = Encoders.text
  decoder = Decoders.text

instance IsScalar Scientific where
  encoder = Encoders.numeric
  decoder = Decoders.numeric

instance IsScalar UUID where
  encoder = Encoders.uuid
  decoder = Decoders.uuid
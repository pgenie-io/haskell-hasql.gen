module Demo.MusicCatalogue.Algebras.Scalar where

import Data.Tagged (Tagged(..), retag, untag)
import qualified Hasql.Encoders as Encoders
import qualified Hasql.Decoders as Decoders

class IsScalar a where
  encoder :: Encoders.Value a
  decoder :: Decoders.Value a

instance IsScalar Int where
  encoder = fromIntegral >$< Encoders.int8
  decoder = fromIntegral <$> Decoders.int8

instance IsScalar Bool where
  encoder = Encoders.bool
  decoder = Decoders.bool

instance IsScalar Text where
  encoder = Encoders.text
  decoder = Decoders.text

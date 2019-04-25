module Data.Config.Color where

import Data.Aeson
import Data.Word (Word8)
import MiniLight
import qualified SDL.Vect as Vect

data Config = Config {
  red :: Int,
  green :: Int,
  blue :: Int,
  alpha :: Int
}

asV4 :: Config -> Vect.V4 Word8
asV4 (Config r g b a) = Vect.V4 r g b a

instance FromJSON Config where
  parseJSON = withObject "color" $ \v -> do
    vec <- fromJSON v
    return $ Config (vec VU.! 0) (vec VU.! 1) (vec VU.! 2) (vec VU.! 3)


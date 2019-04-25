module Data.Config.Size where

import Data.Aeson
import qualified SDL.Vect as Vect

data Config = Config {
  width :: Int,
  height :: Int
}

asV2 :: Config -> Vect.V2 Int
asV2 (Config w h) = Vect.V2 w h

instance FromJSON Config where
  parseJSON = withObject "size" $ \v ->
    Config
      <$> v .:? "width" .!= 0
      <*> v .:? "height" .!= 0

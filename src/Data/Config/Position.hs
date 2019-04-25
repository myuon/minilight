module Data.Config.Position where

import Data.Aeson
import MiniLight
import qualified SDL.Vect as Vect

data Config = Config {
  x :: Int,
  y :: Int
}

asV2 :: Config -> Vect.V2 Int
asV2 (Config x y) = Vect.V2 x y

instance FromJSON Config where
  parseJSON = withObject "position" $ \v ->
    Config
      <$> v .:? "x" .!= 0
      <*> v .:? "y" .!= 0

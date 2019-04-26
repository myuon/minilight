module Data.Config.Color where

import Data.Aeson
import Data.Word (Word8)
import qualified SDL.Vect as Vect

data Config = Config {
  red :: Int,
  green :: Int,
  blue :: Int,
  alpha :: Int
}

asV4 :: Config -> Vect.V4 Word8
asV4 (Config r g b a) = fmap toEnum $ Vect.V4 r g b a

instance FromJSON Config where
  parseJSON = withObject "color" $ \v -> do
    [r,g,b,a] <- fmap (map toEnum) $ v .:? "color" .!= [255, 255, 255, 255]
    return $ Config r g b a

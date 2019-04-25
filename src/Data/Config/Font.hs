module Data.Config.Font where

import Data.Aeson
import Data.Word (Word8)
import MiniLight
import qualified SDL.Vect as Vect

data Config = Config {
  descriptor :: FontDescriptor,
  size :: Int,
  color :: Vect.V4 Word8
}

instance FromJSON Config where
  parseJSON = withObject "font" $ \v -> do
    fontMaybe <- v .:? "font"

    case fontMaybe of
      Nothing -> return $ Config (FontDescriptor "" (FontStyle False False)) 0 (Vect.V4 0 0 0 255)
      Just w -> flip (withObject "font") w $ \v -> do
        family <- v .: "family"
        size <- v .: "size"
        bold <- v .:? "bold" .!= False
        italic <- v .:? "italic" .!= False
        [r,g,b,a] <- v .:? "color" .!= [0, 0, 0, 255]

        return $ Config (FontDescriptor family (FontStyle bold italic)) size (Vect.V4 r g b a)

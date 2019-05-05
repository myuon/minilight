module Data.Config.Font where

import Control.Lens
import Data.Aeson
import Data.Word (Word8)
import MiniLight
import qualified SDL.Font
import qualified SDL.Vect as Vect

data Config = Config {
  descriptor :: FontDescriptor,
  size :: Int,
  color :: Vect.V4 Word8
}

makeLensesWith classyRules_ ''Config

instance FromJSON Config where
  parseJSON = withObject "font" $ \v -> do
    family <- v .:? "family" .!= ""
    size <- v .:? "size" .!= 0
    bold <- v .:? "bold" .!= False
    italic <- v .:? "italic" .!= False
    [r,g,b,a] <- v .:? "color" .!= [0, 0, 0, 255]

    return $ Config (FontDescriptor family (FontStyle bold italic)) size (Vect.V4 r g b a)

-- | Load a system font from 'Config' type.
loadFontFrom :: Config -> MiniLight SDL.Font.Font
loadFontFrom conf = loadFont (descriptor conf) (size conf)

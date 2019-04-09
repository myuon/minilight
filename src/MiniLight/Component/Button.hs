module MiniLight.Component.Button where

import Data.Aeson
import qualified Data.Text as T
import Data.Word (Word8)
import MiniLight.Component.Types
import MiniLight.Figure
import MiniLight.Light
import qualified SDL.Font
import qualified SDL.Vect as Vect

data Button = Button {
  font :: SDL.Font.Font,
  textTexture :: Figure,
  config :: Config
}

instance ComponentUnit Button where
  update = return

  figures comp = do
    return [
      textTexture comp
      ]

data Config = Config {
  size :: Vect.V2 Int,
  label :: T.Text,
  color :: Vect.V4 Word8,
  fontConf :: FontDescriptor,
  fontSize :: Int
}

instance FromJSON Config where
  parseJSON = withObject "config" $ \v -> do
    size <- withObject "font" (\v -> Vect.V2 <$> v .: "width" <*> v .: "height") =<< v .: "size"
    label <- v .: "label"
    [r,g,b,a] <- v .:? "color" .!= [255, 255, 255, 255]
    (fontConf, fontSize) <- (v .: "font" >>=) $ withObject "font" $ \v -> do
      family <- v .: "family"
      size <- v .: "size"
      bold <- v .:? "bold" .!= False
      italic <- v .:? "italic" .!= False

      return $ (FontDescriptor family (FontStyle bold italic), size)

    return $ Config size label (Vect.V4 r g b a) fontConf fontSize

new :: Config -> MiniLight Button
new conf = do
  font        <- loadFont (fontConf conf) (fontSize conf)
  textTexture <- text font (color conf) $ label conf

  return $ Button {font = font, textTexture = textTexture, config = conf}

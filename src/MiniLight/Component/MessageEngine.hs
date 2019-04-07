module MiniLight.Component.MessageEngine where

import Control.Monad.State
import Data.Aeson
import qualified Data.Text as T
import Data.Word (Word8)
import Lens.Micro.Mtl
import MiniLight.Component.Types
import MiniLight.Figure
import MiniLight.Light
import qualified SDL
import qualified SDL.Font
import qualified SDL.Vect as Vect

data MessageEngine = MessageEngine {
  font :: SDL.Font.Font,
  counter :: Int,
  rendered :: Int,
  textTexture :: FigureData,
  finished :: Bool,
  config :: Config
}

instance ComponentUnit MessageEngine where
  update = execStateT $ do
    comp <- get

    unless (finished comp) $ do
      when (counter comp `mod` 10 == 0) $ do
        id %= (\c -> c { rendered = rendered c + 1 })

        comp <- get
        when (rendered comp == T.length (messages (config comp))) $ do
          id %= (\c -> c { finished = True })

      id %= (\c -> c { counter = counter c + 1 })

  figures comp = do
    (w, h) <- SDL.Font.size (font comp) (T.take (rendered comp) $ messages (config comp))

    return [
      colorize (color (config comp)) $ clip (SDL.Rectangle 0 (Vect.V2 w h)) $ figureOf $ textTexture comp
      ]

data Config = Config {
  messages :: T.Text,
  static :: Bool,
  color :: Vect.V4 Word8,
  fontConf :: FontDescriptor,
  fontSize :: Int
}

instance FromJSON Config where
  parseJSON = withObject "config" $ \v -> do
    messages <- v .: "messages"
    static <- v .:? "static" .!= False
    [r,g,b,a] <- v .:? "color" .!= [255, 255, 255, 255]
    (fontConf, size) <- (v .: "font" >>=) $ withObject "font" $ \v -> do
      family <- v .: "family"
      size <- v .: "size"
      bold <- v .:? "bold" .!= False
      italic <- v .:? "italic" .!= False

      return $ (FontDescriptor family (FontStyle bold italic), size)

    return $ Config messages static (Vect.V4 r g b a) fontConf size

new :: Config -> MiniLight MessageEngine
new conf = do
  font        <- loadFont (fontConf conf) (fontSize conf)
  textTexture <- freeze $ text font $ messages conf

  return $ MessageEngine
    { font        = font
    , counter     = 0
    , rendered    = if static conf then T.length (messages conf) else 0
    , textTexture = textTexture
    , finished    = static conf
    , config      = conf
    }

module MiniLight.Component.MessageEngine where

import Control.Monad.State
import Data.Aeson
import qualified Data.Text as T
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
  message :: T.Text,
  rendered :: Int,
  textTexture :: Figure,
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
        when (rendered comp == T.length (message comp)) $ do
          id %= (\c -> c { finished = True })

      id %= (\c -> c { counter = counter c + 1 })

  figures comp = do
    (w, h) <- SDL.Font.size (font comp) (T.take (rendered comp) $ message comp)

    return [
      colorize (Vect.V4 255 255 255 255) $ clip (SDL.Rectangle 0 (Vect.V2 w h)) $ textTexture comp
      ]

data Config = Config {
  messages :: T.Text,
  static :: Bool
}

instance FromJSON Config where
  parseJSON = withObject "config" $ \v -> do
    messages <- v .: "messages"
    static <- v .:? "static" .!= False

    return $ Config messages static

new :: SDL.Font.Font -> Config -> MiniLight MessageEngine
new font conf = do
  return $ MessageEngine
    { font        = font
    , counter     = 0
    , message     = messages conf
    , rendered    = if static conf then T.length (messages conf) else 0
    , textTexture = text font $ messages conf
    , finished    = static conf
    , config      = conf
    }

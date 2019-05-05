module Data.Component.MessageEngine where

import Control.Lens
import Control.Lens.TH.Rules
import Control.Monad.State
import Data.Aeson
import qualified Data.Config.Font as Font
import qualified Data.Text as T
import MiniLight
import qualified SDL
import qualified SDL.Font
import qualified SDL.Vect as Vect

data Config = Config {
  messages :: T.Text,
  static :: Bool,
  font :: Font.Config
}

makeLensesWith lensRules_ ''Config

instance FromJSON Config where
  parseJSON = withObject "config" $ \v -> do
    messages <- v .: "messages"
    static <- v .:? "static" .!= False
    font <- v .: "font"

    return $ Config messages static font

data MessageEngine = MessageEngine {
  fontData :: SDL.Font.Font,
  counter :: Int,
  rendered :: Int,
  textTexture :: Figure,
  finished :: Bool,
  config :: Config
}

makeLensesWith lensRules_ ''MessageEngine

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
    (w, h) <- SDL.Font.size (comp ^. _fontData) (T.take (rendered comp) $ messages (config comp))

    return [
      clip (SDL.Rectangle 0 (Vect.V2 w h)) $ textTexture comp
      ]

new :: Config -> MiniLight MessageEngine
new conf = do
  font        <- Font.loadFontFrom (font conf)
  textTexture <- text font (conf ^. _font ^. Font._color) $ messages conf

  return $ MessageEngine
    { fontData    = font
    , counter     = 0
    , rendered    = if static conf then T.length (messages conf) else 0
    , textTexture = textTexture
    , finished    = static conf
    , config      = conf
    }

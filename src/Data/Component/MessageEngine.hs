module Data.Component.MessageEngine where

import Control.Lens
import Control.Lens.TH.Rules
import Control.Monad.State
import Data.Aeson hiding ((.=))
import qualified Data.Config.Font as Font
import qualified Data.Text as T
import qualified Data.Vector as V
import Data.Typeable
import MiniLight
import qualified SDL
import qualified SDL.Font
import qualified SDL.Vect as Vect

-- | 'MessageEngine' configuration. If @static@ enabled, only the first page will be rendered.
data Config = Config {
  messages :: V.Vector T.Text,  -- ^ paged messages
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
  page :: Int,
  textCounter :: Int,
  textTexture :: Figure,
  finished :: Bool,
  config :: Config
}

makeLensesWith lensRules_ ''MessageEngine

data EngineEvent = NextPage
  deriving Typeable

instance EventType EngineEvent

instance ComponentUnit MessageEngine where
  update = execStateT $ do
    comp <- get

    unless (finished comp) $ do
      when (counter comp `mod` 10 == 0) $ do
        id %= (\c -> c { textCounter = textCounter c + 1 })

        comp <- get
        when (comp ^. _textCounter == T.length ((config comp ^. _messages) V.! (comp ^. _page))) $ do
          id %= (\c -> c { finished = True })

      id %= (\c -> c { counter = counter c + 1 })

  figures comp = do
    (w, h) <- SDL.Font.size (comp ^. _fontData) (T.take (comp ^. _textCounter) $ (config comp ^. _messages) V.! (comp ^. _page))

    return [
      clip (SDL.Rectangle 0 (Vect.V2 w h)) $ textTexture comp
      ]

  useCache c1 c2 = page c1 == page c2 && textCounter c1 == textCounter c2

  onSignal ev c = view uidL >>= \u -> go (ev,u) c
    where
      go (uncurry asSignal -> Just NextPage) = execStateT $ do
        _page %= (+1)
        _textCounter .= 0

        font <- use _fontData
        fontColor <- use $ _config . _font . Font._color
        p <- use _page
        messages <- use $ _config . _messages
        tex <- lift $ liftMiniLight $ text font fontColor (messages V.! p)
        _textTexture .= tex

      go _ = return

new :: Config -> MiniLight MessageEngine
new conf = do
  font        <- Font.loadFontFrom (font conf)
  textTexture <- text font (conf ^. _font ^. Font._color) $ messages conf V.! 0

  return $ MessageEngine
    { fontData    = font
    , counter     = 0
    , page        = 0
    , textCounter = if static conf then T.length (messages conf V.! 0) else 0
    , textTexture = textTexture
    , finished    = static conf
    , config      = conf
    }

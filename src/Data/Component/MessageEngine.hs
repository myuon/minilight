module Data.Component.MessageEngine where

import Control.Lens
import Control.Lens.TH.Rules
import Control.Monad.State
import Control.Monad.Catch (MonadMask)
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
  currentMessages :: V.Vector T.Text,
  config :: Config
}

makeLensesWith lensRules_ ''MessageEngine

data EngineEvent where
  NextPage :: EngineEvent
  SetMessage
    :: [T.Text]  -- ^ pages messages
    -> EngineEvent
  deriving Typeable

instance EventType EngineEvent where
  getEventType NextPage = "next-page"
  getEventType (SetMessage _) = "set-message"

instance ComponentUnit MessageEngine where
  update = execStateT $ do
    fin <- use _finished
    unless fin $ do
      cnt <- use _counter
      when (cnt `mod` 10 == 0) $ do
        _textCounter %= (+1)

        tc <- use _textCounter
        p <- use _page
        messages <- use _currentMessages
        when (p == V.length messages - 1 && tc == T.length (messages V.! p)) $ do
          _finished .= True

      _counter %= (+1)

  figures comp = do
    let messages = comp ^. _currentMessages
    (w, h) <- SDL.Font.size (comp ^. _fontData) (T.take (comp ^. _textCounter) $ messages V.! (comp ^. _page))

    return [
      clip (SDL.Rectangle 0 (Vect.V2 w h)) $ textTexture comp
      ]

  useCache c1 c2 = page c1 == page c2 && textCounter c1 == textCounter c2

  onSignal ev = execStateT $ case asSignal ev of
    Just NextPage -> do
      fin <- use _finished
      unless fin $ do
        _page %= (+1)
        _textCounter .= 0

        font <- use _fontData
        fontColor <- use $ _config . _font . Font._color
        p <- use _page
        messages <- use _currentMessages
        tex <- lift $ liftMiniLight $ text font fontColor (messages V.! p)
        _textTexture .= tex
    Just (SetMessage ms) -> do
      let vs = V.fromList ms
      _counter .= 0
      _page .= 0

      st <- use $ _config . _static
      _textCounter .= if st then T.length (vs V.! 0) else 0

      font        <- use _fontData
      fontColor   <- use $ _config . _font . Font._color
      tex         <- lift $ liftMiniLight $ text font fontColor $ vs V.! 0
      _textTexture .= tex
      _finished    .= st
      _currentMessages .= vs
    _ -> return ()

new :: Config -> MiniLight MessageEngine
new conf = do
  font        <- Font.loadFontFrom (font conf)
  textTexture <- text font (conf ^. _font ^. Font._color) $ messages conf V.! 0

  return $ MessageEngine
    { fontData        = font
    , counter         = 0
    , page            = 0
    , textCounter = if static conf then T.length (messages conf V.! 0) else 0
    , textTexture     = textTexture
    , finished        = static conf
    , currentMessages = messages conf
    , config          = conf
    }

wrapSignal
  :: ( HasLightEnv env
     , HasLoopEnv env
     , HasComponentEnv env
     , MonadIO m
     , MonadMask m
     )
  => (Lens' c MessageEngine)
  -> (Event -> c -> LightT env m c)
  -> (Event -> c -> LightT env m c)
wrapSignal lens f ev = execStateT $ do
  zoom lens $ do
    st  <- use id
    st' <- lift $ onSignal ev st
    id .= st'

  st  <- use id
  st' <- lift $ f ev st
  id .= st'

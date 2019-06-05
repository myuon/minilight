module Data.Component.MessageLayer where

import Control.Lens
import Control.Lens.TH.Rules
import Control.Monad.State
import Data.Aeson
import Data.Typeable (Typeable)
import Linear
import MiniLight
import qualified Data.Component.Basic as Basic
import qualified Data.Component.Layer as CLayer
import qualified Data.Component.AnimationLayer as CAnim
import qualified Data.Component.MessageEngine as CME
import qualified SDL.Vect as Vect

data Config = Config {
  basic :: Basic.Config,
  engine :: CME.Config,
  window :: CLayer.Config,
  next :: CAnim.Config
}

instance FromJSON Config where
  parseJSON = withObject "config" $ \v -> do
    layerConf <- parseJSON =<< v .: "window"
    nextConf <- parseJSON =<< v .: "next"
    messageEngineConf <- parseJSON =<< v .: "engine"

    return $ Config (layerConf ^. Basic.config) messageEngineConf layerConf nextConf

data MessageLayer = MessageLayer {
  messageEngine :: CME.MessageEngine,
  layer :: CLayer.Layer,
  cursor :: CAnim.AnimationLayer,
  config :: Config
}

makeLensesWith lensRules_ ''Config
makeLensesWith lensRules_ ''MessageLayer

instance Basic.HasConfig Config where
  config = _basic

engineL :: Lens' MessageLayer CME.MessageEngine
engineL = lens messageEngine (\s a -> s { messageEngine = a })

cursorL :: Lens' MessageLayer CAnim.AnimationLayer
cursorL = lens cursor (\s a -> s { cursor = a })

data MessageLayerEvent where
  Finish :: MessageLayerEvent
  deriving Typeable

instance EventType MessageLayerEvent where
  getEventType Finish = "finish"

instance ComponentUnit MessageLayer where
  update = execStateT $ do
    zoom engineL $ do
      c <- use id
      id <~ lift (update c)

    zoom cursorL $ do
      c <- use id
      id <~ lift (update c)

  figures comp = do
    baseLayer <- figures $ layer comp
    cursorLayer <- figures $ cursor comp
    textLayer <- figures $ messageEngine comp

    let cursorSize = CAnim.tileSize (cursor comp)
    let windowSize = Basic.size $ CLayer.basic $ window $ config comp
    let position = Basic.position $ CLayer.basic $ window $ config comp

    return
      $ baseLayer
      ++ map (translate (position + Vect.V2 20 10)) textLayer
      ++ map (translate (position + Vect.V2 ((windowSize ^. _x - cursorSize ^. _x) `div` 2) (windowSize ^. _y - cursorSize ^. _y))) cursorLayer

  onSignal
    = Basic.wrapSignal (_config . Basic.config)
    $ CME.wrapSignal _messageEngine
    $ \ev -> execStateT $ case asSignal ev of
      Just (Basic.MouseReleased _) -> do
        me <- use _messageEngine

        if me ^. CME._finished
          then lift $ emitGlobally Finish
          else lift $ emitGlobally CME.NextPage
      _ -> return ()

new :: Config -> MiniLight MessageLayer
new conf = do
  engine <- CME.new (engine conf)
  layer  <- CLayer.newNineTile (window conf)
  cursor <- CAnim.new (next conf)

  return $ MessageLayer
    { messageEngine = engine
    , layer         = layer
    , cursor        = cursor
    , config        = conf
    }

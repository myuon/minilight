module Data.Component.Selection where

import Control.Lens
import Control.Lens.TH.Rules
import Control.Monad.State
import Data.Aeson hiding ((.=))
import qualified Data.Config.Font as Font
import qualified Data.HashMap.Strict as HM
import qualified Data.Text as T
import Data.Typeable (Typeable)
import qualified Data.Vector as V
import Linear
import MiniLight
import qualified SDL.Font
import qualified SDL.Vect as Vect
import qualified Data.Component.Basic as Basic
import qualified Data.Component.Layer as Layer

data Config = Config {
  basic :: Basic.Config,
  labels :: V.Vector T.Text,
  fontConfig :: Font.Config,
  image :: FilePath
}

instance FromJSON Config where
  parseJSON = withObject "selection" $ \v ->
    Config
      <$> parseJSON (Object v)
      <*> v .:? "labels" .!= V.empty
      <*> (parseJSON =<< (v .: "font"))
      <*> v .: "image"

data Selection = Selection {
  layer :: Layer.Layer,
  font :: SDL.Font.Font,
  hover :: Maybe Int,
  config :: Config
}

makeLensesWith lensRules_ ''Config
makeLensesWith lensRules_ ''Selection

instance Basic.HasConfig Config where
  config = _basic

instance ComponentUnit Selection where
  update = return

  figures comp = do
    let p = Vect.V2 15 10
    textTextures <- V.forM (V.indexed $ labels $ comp ^. _config) $ \(i,label) -> liftMiniLight $ fmap (translate (p + Vect.V2 0 (i * 30))) $ text (font comp) (Font.color $ fontConfig $ comp ^. _config) label
    base <- figures (layer comp)
    highlight <- liftMiniLight $ rectangleFilled (Vect.V4 240 240 240 40) $ _y .~ 30 $ comp ^. _config . Basic._size

    return $ Basic.wrapFigures (comp ^. _config . Basic.config) $ base
      ++ (translate (Vect.V2 0 (maybe 0 id (hover comp) * 30 + p ^. _y)) highlight
      : V.toList textTextures)

  useCache c1 c2 = c1 ^. _config . Basic._disabled == c2 ^. _config . Basic._disabled && c1 ^. _hover == c2 ^. _hover

  onSignal = Basic.wrapSignal (_config . Basic.config) $ \ev sel -> flip execStateT sel $ do
    case asSignal ev of
      Just (Basic.MouseOver pos) | (pos ^. _y) `div` 30 <= V.length (sel ^. _config . _labels) - 1 -> do
        _hover .= Just ((pos ^. _y) `div` 30)
      Just (Basic.MouseReleased pos) | (pos ^. _y) `div` 30 <= V.length (sel ^. _config . _labels) - 1 -> do
        lift $ emitGlobally $ Select ((pos ^. _y) `div` 30)
      _ -> return ()

  -- OMG
  beforeClearCache _ [] = return ()
  beforeClearCache _ figs = mapM_ freeFigure $ tail figs

data SelectionEvent
  = Select Int
  deriving (Typeable)

instance EventType SelectionEvent where
  getEventType (Select _) = "select"

  getEventProperties (Select n) = HM.fromList [("index", Number $ fromIntegral n)]

new :: Config -> MiniLight Selection
new conf = do
  font  <- Font.loadFontFrom (fontConfig conf)
  layer <- Layer.newNineTile $ Layer.Config
    (Basic.defConfig { Basic.size = Basic.size $ basic conf })
    (image conf)
  return
    $ Selection {font = font, config = conf, hover = Nothing, layer = layer}

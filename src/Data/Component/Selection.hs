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
  conf :: Config
}

makeLensesWith lensRules_ ''Config
makeLensesWith lensRules_ ''Selection

hoverL :: Lens' Selection (Maybe Int)
hoverL = lens hover (\env r -> env { hover = r })

instance ComponentUnit Selection where
  update = return

  figures comp = do
    let p = Vect.V2 15 10
    textTextures <- V.forM (V.indexed $ labels $ conf comp) $ \(i,label) -> liftMiniLight $ fmap (translate (p + Vect.V2 0 (i * 30))) $ text (font comp) (Font.color $ fontConfig $ conf comp) label
    base <- figures (layer comp)
    highlight <- liftMiniLight $ rectangleFilled (Vect.V4 240 240 240 40) $ _y .~ 30 $ Basic.size (basic (conf comp))

    return $ Basic.wrapFigures (basic $ conf comp) $ base
      ++ (translate (Vect.V2 0 (maybe 0 id (hover comp) * 30 + p ^. _y)) highlight
      : V.toList textTextures)

  -- first-aid for caching
  useCache c1 c2 = Basic.disabled (basic (conf c1)) == Basic.disabled (basic (conf c2)) && hover c1 == hover c2

  onSignal = Basic.wrapSignal (basic . conf) $ \ev sel -> flip execStateT sel $ do
    uid <- view _uid

    case ev `asSignal` uid of
      Just (Basic.MouseOver pos) | (pos ^. _y) `div` 30 <= V.length (labels (conf sel)) - 1 -> do
        hoverL .= Just ((pos ^. _y) `div` 30)
      Just (Basic.MouseReleased pos) | (pos ^. _y) `div` 30 <= V.length (labels (conf sel)) - 1 -> do
        lift $ emit $ Select ((pos ^. _y) `div` 30)
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
  return $ Selection {font = font, conf = conf, hover = Nothing, layer = layer}

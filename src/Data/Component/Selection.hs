module Data.Component.Selection where

import Control.Lens
import Control.Monad.State
import Data.Aeson hiding ((.=))
import qualified Data.Config.Font as Font
import qualified Data.Text as T
import qualified Data.Vector as V
import Linear
import MiniLight
import qualified SDL.Font
import qualified SDL.Vect as Vect
import qualified Data.Component.Basic as Basic
import qualified Data.Component.Layer as Layer

data Selection = Selection {
  layer :: Layer.Layer,
  font :: SDL.Font.Font,
  hover :: Maybe Int,
  conf :: Config
}

hoverL :: Lens' Selection (Maybe Int)
hoverL = lens hover (\env r -> env { hover = r })

instance ComponentUnit Selection where
  update = return

  figures comp = do
    let p = Vect.V2 15 10
    textTextures <- V.forM (V.indexed $ labels $ conf comp) $ \(i,label) -> liftMiniLight $ fmap (translate (p + Vect.V2 0 (i * 30))) $ text (font comp) (Font.color $ fontConfig $ conf comp) label
    base <- figures (layer comp)
    highlight <- liftMiniLight $ rectangleFilled (Vect.V4 240 240 240 40) $ _y .~ 30 $ Basic.size (basic (conf comp))

    return $ base
      ++ map (translate (Basic.position $ basic $ conf comp)) (translate (Vect.V2 0 (maybe 0 id (hover comp) * 30 + p ^. _y)) highlight
      : V.toList textTextures)

  useCache c1 c2 = hover c1 == hover c2

  onSignal = Basic.wrapSignal (basic . conf) $ \ev sel -> flip execStateT sel $ do
    uid <- view uidL

    case ev `asSignal` uid of
      Just (Basic.MouseOver pos) | (pos ^. _y) `div` 30 <= V.length (labels (conf sel)) - 1 -> do
        hoverL .= Just ((pos ^. _y) `div` 30)
      _ -> return ()

  -- OMG
  beforeClearCache _ figs = mapM_ freeFigure $ tail figs

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

new :: Config -> MiniLight Selection
new conf = do
  font  <- Font.loadFontFrom (fontConfig conf)
  layer <- Layer.newNineTile $ Layer.Config (basic conf) (image conf)
  return $ Selection {font = font, conf = conf, hover = Nothing, layer = layer}

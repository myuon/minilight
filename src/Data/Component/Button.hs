module Data.Component.Button where

import Data.Aeson
import qualified Data.Text as T
import Data.Typeable
import Data.Word (Word8)
import MiniLight
import qualified SDL
import qualified SDL.Font
import qualified SDL.Vect as Vect

data Button = Button {
  font :: SDL.Font.Font,
  config :: Config
}

data ButtonEvent = Click
  deriving Typeable

instance EventType ButtonEvent

instance ComponentUnit Button where
  update = return

  figures comp = do
    textTexture <- liftMiniLight $ text (font comp) (color (config comp)) $ label (config comp)
    base <- liftMiniLight $ rectangleFilled (Vect.V4 200 200 200 255) (getFigureSize textTexture)

    return [
      base,
      textTexture
      ]

  useCache _ _ = True

  onSignal (RawEvent (SDL.Event _ (SDL.MouseButtonEvent (SDL.MouseButtonEventData _ SDL.Released _ _ _ _)))) comp = do
    emit "_" Click
    return comp
  onSignal _ comp = return comp

  beforeClearCache _ figs = mapM_ freeFigure figs

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
  font <- loadFont (fontConf conf) (fontSize conf)
  return $ Button {font = font, config = conf}

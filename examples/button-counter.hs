{-# LANGUAGE OverloadedStrings #-}
import Data.Component.Resolver
import MiniLight
import qualified Data.Text as T
import qualified SDL
import qualified SDL.Font
import qualified SDL.Vect as Vect

data Button = Button {
  font :: SDL.Font.Font,
  counter :: Int
}

instance ComponentUnit Button where
  update = return

  figures comp = do
    textTexture <- liftMiniLight $ text (font comp) (Vect.V4 255 255 255 255) $
      if counter comp == 0 then "Click me!" else "You've clicked " `T.append` T.pack (show (counter comp)) `T.append` " times!"
    base <- liftMiniLight $ rectangleFilled (Vect.V4 60 60 60 255) (getFigureSize textTexture)

    return [
      base,
      textTexture
      ]

  useCache prev now = counter prev == counter now

  onSignal (RawEvent (SDL.Event _ (SDL.MouseButtonEvent (SDL.MouseButtonEventData _ SDL.Released _ _ _ _)))) comp = do
    return $ comp { counter = counter comp + 1 }
  onSignal _ comp = return comp

  beforeClearCache _ figs = mapM_ freeFigure figs

new :: MiniLight Button
new = do
  font <- loadFont (FontDescriptor "IPAGothic" (FontStyle False False)) 22
  return $ Button {font = font, counter = 0}

main :: IO ()
main = do
  runLightT $ do
    button <- newComponent =<< new

    runMainloop
      ( (defConfig resolver) { appConfigFile        = Nothing
                             , additionalComponents = [button]
                             }
      )
      ()
      (\_ -> return)

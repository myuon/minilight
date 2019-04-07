{-# LANGUAGE OverloadedStrings #-}
module Main where

import Control.Monad.State
import Control.Monad.IO.Class
import Data.Aeson
import qualified Data.ByteString as B
import qualified Data.Knob as Knob
import Data.List (transpose)
import qualified Data.Vector as V
import qualified Data.Vector.Mutable as VM
import Data.WAVE
import Graphics.Text.TrueType
import MiniLight
import qualified MiniLight.Component.Layer as CLayer
import qualified MiniLight.Component.MessageEngine as CME
import System.IO (IOMode(..), hClose)
import qualified SDL.Mixer

gen_sine :: Double -> Double -> Int -> [WAVESample]
gen_sine hz secs fr =
  map doubleToSample
    $ [ ( sin (2 * pi * hz * fromIntegral t / fromIntegral fr)
        + 0.5
        * sin (2 * pi * (2 * hz) * fromIntegral t / fromIntegral fr)
        + 0.25
        * sin (2 * pi * (3 * hz) * fromIntegral t / fromIntegral fr)
        + 0.125
        * sin (2 * pi * (4 * hz) * fromIntegral t / fromIntegral fr)
        )
          / (1.0 + 0.5 + 0.25 + 0.125)
      | t <- [0 .. floor (fromIntegral fr * secs)]
      ]

pulse :: WAVE
pulse = WAVE
  { waveHeader  = WAVEHeader
    { waveNumChannels   = 1
    , waveFrameRate     = frameRate
    , waveBitsPerSample = 16
    , waveFrames        = Nothing
    }
  , waveSamples = transpose [samples]
  }
 where
  samples   = gen_sine 440 2 frameRate
  frameRate = 44100

data Game = Game {
  components :: VM.IOVector Component,
  sound :: SDL.Mixer.Chunk
}

mainloop :: LoopState -> StateT Game MiniLight ()
mainloop _ = do
  game <- get

  forM_ [0 .. VM.length (components game) - 1] $ \i -> lift $ do
    comp <- liftIO $ VM.read (components game) i
    draw comp

  forM_ [0 .. VM.length (components game) - 1] $ \i -> lift $ do
    comp  <- liftIO $ VM.read (components game) i
    comp' <- update comp
    liftIO $ VM.write (components game) i comp'

  return ()

foldResult :: (String -> b) -> (a -> b) -> Result a -> b
foldResult f g r = case r of
  Error   err -> f err
  Success a   -> g a

main :: IO ()
main = do
  fonts  <- buildCache
  knob   <- Knob.newKnob ""
  handle <- Knob.newFileHandle knob "<buffer>" WriteMode
  hPutWAVE handle pulse
  hClose handle

  SDL.Mixer.withAudio SDL.Mixer.defaultAudio 4096 $ do
    buffer <- Knob.getContents knob
    sound  <- SDL.Mixer.decode buffer
    SDL.Mixer.setVolume 10 sound
--    SDL.Mixer.play sound

    runLightT id
      $ withFont
          ( (\(Just x) -> x)
          $ findFontInCache fonts
          $ FontDescriptor "IPAGothic"
          $ FontStyle False False
          )
      $ \font -> do
          comps <-
            (liftIO . V.thaw . V.fromList =<<)
            $ loadAppConfig "resources/app.yml"
            $ \name props -> case name of
                "message-engine" -> Component
                  <$> CME.new font (foldResult error id $ fromJSON props)
          runMainloop (LoopConfig {watchKeys = Nothing})
                      (Game {components = comps, sound = sound})
                      (\st -> execStateT $ mainloop st)



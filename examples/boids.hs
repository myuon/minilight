{-# LANGUAGE OverloadedStrings #-}
import MiniLight
import Control.Monad.State
import qualified Data.Vector as V
import qualified Data.Vector.Mutable as VM
import Lens.Micro
import Linear (_x, _y)
import qualified System.Random.MWC as Random
import qualified SDL.Vect as Vect

average :: (Fractional a, Num a) => V.Vector a -> a
average xs = (/ fromIntegral (V.length xs)) $ V.sum xs

data Object = Object {
  position :: Vect.V2 Double,
  velocity :: Vect.V2 Double
}

distance :: Object -> Object -> Double
distance o1 o2 = Vect.distance (position o1) (position o2)

data Game = Game {
  objects :: VM.IOVector Object,
  pic :: Figure
}

numberOfObjects = 100

width :: Int
width = 800

height :: Int
height = 600

mainloop :: StateT Game MiniLight ()
mainloop = do
  Game { objects = objects, pic = pic } <- get

  lift $ (renders =<<) $ forM [0 .. VM.length objects - 1] $ \i -> do
    object <- liftIO $ VM.read objects i
    return
      $ translate (fmap floor $ position object)
      $ rotate (let Vect.V2 x y = velocity object in pi - atan2 x y)
      $ pic

  lift $ forM_ [0 .. VM.length objects - 1] $ \i -> do
    object <- liftIO $ VM.read objects i

    let position'     = position object + 0.2 Vect.*^ velocity object
    let Vect.V2 vx vy = velocity object

    liftIO $ VM.write objects i $ do
      object
        { position = position'
        , velocity = Vect.V2
          ( if position' ^. _x < 0
            then abs vx
            else if position' ^. _x > fromIntegral width then -abs vx else vx
          )
          ( if position' ^. _y < 0
            then abs vy
            else if position' ^. _y > fromIntegral height then -abs vy else vy
          )
        }

  objs <- liftIO $ V.freeze objects
  lift $ forM_ [0 .. VM.length objects - 1] $ \i -> do
    let radius = 20
    let neighbors = V.filter
          ( \o ->
            0 < distance o (objs V.! i) && distance o (objs V.! i) < radius
          )
          objs

    object <- liftIO $ VM.read objects i

    when (not $ null neighbors) $ do
      -- separation
      let v1 = average $ V.map
            ( \n ->
              let diff = position object - position n
              in  (1 / Vect.dot diff diff) Vect.*^ diff
            )
            neighbors

      -- alignment
      let v2     = average $ V.map velocity neighbors

      -- cohesion
      let center = average $ V.map position neighbors
      let v3     = (1 / radius / radius) Vect.*^ (center - position object)

      liftIO $ VM.write objects i $ object
        { velocity = Vect.normalize $ velocity object + v1 + v2 + v3
        }

main :: IO ()
main = do
  vec <- Random.withSystemRandom . Random.asGenIO $ \gen ->
    Random.uniformVector gen numberOfObjects :: IO
        (V.Vector (Double, Double, Double, Double))
  objs <- V.thaw $ V.map
    ( \(p1, p2, v1, v2) -> Object
      ( fmap fromIntegral $ Vect.V2
        (floor (p1 * fromIntegral width) `mod` width)
        (floor (p2 * fromIntegral height) `mod` height)
      )
      (Vect.normalize $ Vect.V2 v1 v2)
    )
    vec

  runLightT id $ do
    pic <- triangleOutline (Vect.V4 100 100 100 255) (Vect.V2 10 20)

    runMainloop
      (defConfig { appConfigFile = Nothing, additionalComponents = [] })
      (Game {objects = objs, pic = pic})
      (\_ -> execStateT mainloop)

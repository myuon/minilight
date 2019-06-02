{-| This module provides the default resolver for pre-defined components.
-}
module Data.Component.Resolver (
  resolver,
  extendResolver,
) where

import Control.Monad
import Data.Aeson
import qualified Data.Text as T
import MiniLight
import qualified Data.Component.AnimationLayer as AnimationLayer
import qualified Data.Component.Button as Button
import qualified Data.Component.Layer as Layer
import qualified Data.Component.MessageEngine as MessageEngine
import qualified Data.Component.MessageLayer as MessageLayer
import qualified Data.Component.Selection as Selection

foldResult :: (String -> b) -> (a -> b) -> Result a -> b
foldResult f g r = case r of
  Error   err -> f err
  Success a   -> g a

resultM
  :: Result a
  -> (a -> MiniLight Component)
  -> MiniLight (Either String Component)
resultM r m = foldResult (return . Left) (fmap Right . m) r

-- | Pre-defined resolver supports all components in this library.
resolver :: Resolver
resolver name uid props = case name of
  "animation-layer" ->
    resultM (fromJSON props) $ newComponent uid <=< AnimationLayer.new
  "button" -> resultM (fromJSON props) $ newComponent uid <=< Button.new
  "layer"  -> resultM (fromJSON props) $ newComponent uid <=< Layer.new
  "message-engine" ->
    resultM (fromJSON props) $ newComponent uid <=< MessageEngine.new
  "message-layer" ->
    resultM (fromJSON props) $ newComponent uid <=< MessageLayer.new
  "tiled-layer" ->
    resultM (fromJSON props) $ newComponent uid <=< Layer.newNineTile
  "selection" -> resultM (fromJSON props) $ newComponent uid <=< Selection.new
  _           -> return $ Left $ "Unsupported component: " ++ T.unpack name

extendResolver
  :: (FromJSON a, ComponentUnit c)
  => T.Text  -- ^ Name
  -> (a -> MiniLight c)  -- ^ Constructor of the component
  -> Resolver  -- ^ Old resolver
  -> Resolver
extendResolver newName func resolver name uid props = if name == newName
  then resultM (fromJSON props) $ newComponent uid <=< func
  else resolver name uid props

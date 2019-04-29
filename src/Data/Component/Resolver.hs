{-| This module provides the default resolver for pre-defined components.
-}
module Data.Component.Resolver (
  resolver,
  foldResult,
) where

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

-- | Pre-defined resolver supports all components in this library.
resolver :: Resolver
resolver name props = case name of
  "animation-layer" ->
    newComponent =<< AnimationLayer.new (foldResult error id $ fromJSON props)
  "button" ->
    newComponent =<< Button.new (foldResult error id $ fromJSON props)
  "layer" -> newComponent =<< Layer.new (foldResult error id $ fromJSON props)
  "message-engine" ->
    newComponent =<< MessageEngine.new (foldResult error id $ fromJSON props)
  "message-layer" ->
    newComponent =<< MessageLayer.new (foldResult error id $ fromJSON props)
  "tiled-layer" ->
    newComponent =<< Layer.newNineTile (foldResult error id $ fromJSON props)
  "selection" ->
    newComponent =<< Selection.new (foldResult error id $ fromJSON props)
  _ -> error $ T.unpack $ "Component not defined: `" <> name <> "`"

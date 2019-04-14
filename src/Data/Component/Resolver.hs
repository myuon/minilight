{-| This module provides the default resolver for pre-defined components.
-}
module Data.Component.Resolver (
  resolver,
) where

import qualified Data.Aeson as Aeson
import qualified Data.Text as T
import MiniLight
import qualified Data.Component.AnimationLayer as AnimationLayer
import qualified Data.Component.Button as Button
import qualified Data.Component.Layer as Layer
import qualified Data.Component.MessageEngine as MessageEngine
import qualified Data.Component.MessageLayer as MessageLayer

foldResult :: (String -> b) -> (a -> b) -> Aeson.Result a -> b
foldResult f g r = case r of
  Aeson.Error   err -> f err
  Aeson.Success a   -> g a

-- | Pre-defined resolver supports all components in this library.
resolver :: Resolver
resolver name props = case name of
  "animation-layer" -> newComponent
    =<< AnimationLayer.new (foldResult error id $ Aeson.fromJSON props)
  "button" ->
    newComponent =<< Button.new (foldResult error id $ Aeson.fromJSON props)
  "layer" ->
    newComponent =<< Layer.new (foldResult error id $ Aeson.fromJSON props)
  "message-engine" -> newComponent
    =<< MessageEngine.new (foldResult error id $ Aeson.fromJSON props)
  "message-layer" -> newComponent
    =<< MessageLayer.new (foldResult error id $ Aeson.fromJSON props)
  "tiled-layer" -> newComponent
    =<< Layer.newNineTile (foldResult error id $ Aeson.fromJSON props)
  _ -> error $ T.unpack $ "Component not defined: `" <> name <> "`"


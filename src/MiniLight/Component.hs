module MiniLight.Component (
  module MiniLight.Component.Types,
  loadAppConfig,

  Resolver,
  defResolver
) where

import qualified Data.Aeson as Aeson
import qualified Data.Text as T
import MiniLight.Light
import MiniLight.Component.Types
import MiniLight.Component.Loader
import qualified MiniLight.Component.AnimationLayer as AnimationLayer
import qualified MiniLight.Component.Button as Button
import qualified MiniLight.Component.Layer as Layer
import qualified MiniLight.Component.MessageEngine as MessageEngine
import qualified MiniLight.Component.MessageLayer as MessageLayer

foldResult :: (String -> b) -> (a -> b) -> Aeson.Result a -> b
foldResult f g r = case r of
  Aeson.Error   err -> f err
  Aeson.Success a   -> g a

type Resolver = T.Text -> Aeson.Value -> MiniLight Component

defResolver :: Resolver
defResolver name props = case name of
  "animation-layer" -> newComponent
    <$> AnimationLayer.new (foldResult error id $ Aeson.fromJSON props)
  "button" ->
    newComponent <$> Button.new (foldResult error id $ Aeson.fromJSON props)
  "layer" ->
    newComponent <$> Layer.new (foldResult error id $ Aeson.fromJSON props)
  "message-engine" -> newComponent
    <$> MessageEngine.new (foldResult error id $ Aeson.fromJSON props)
  "message-layer" -> newComponent
    <$> MessageLayer.new (foldResult error id $ Aeson.fromJSON props)
  "tiled-layer" -> newComponent
    <$> Layer.newNineTile (foldResult error id $ Aeson.fromJSON props)
  _ -> error $ T.unpack $ "Component not defined: `" <> name <> "`"

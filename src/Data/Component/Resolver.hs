{-| This module provides the default resolver for pre-defined components.
-}
module Data.Component.Resolver (
  ResolverError (..),
  resolver,
  foldResult,
) where

import Control.Monad.Catch
import Data.Aeson
import qualified Data.Text as T
import MiniLight
import qualified Data.Component.AnimationLayer as AnimationLayer
import qualified Data.Component.Button as Button
import qualified Data.Component.Layer as Layer
import qualified Data.Component.MessageEngine as MessageEngine
import qualified Data.Component.MessageLayer as MessageLayer
import qualified Data.Component.Selection as Selection

data ResolverError = ComponentCreationFailed T.Text
  deriving (Eq, Show)

instance Exception ResolverError

foldResult :: (String -> b) -> (a -> b) -> Result a -> b
foldResult f g r = case r of
  Error   err -> f err
  Success a   -> g a

-- | Pre-defined resolver supports all components in this library.
resolver :: Resolver
resolver name uid props = case name of
  "animation-layer" ->
    newComponent uid =<< AnimationLayer.new =<< asSuccess props
  "button" -> newComponent uid =<< Button.new =<< asSuccess props
  "layer"  -> newComponent uid =<< Layer.new =<< asSuccess props
  "message-engine" ->
    newComponent uid =<< MessageEngine.new =<< asSuccess props
  "message-layer" -> newComponent uid =<< MessageLayer.new =<< asSuccess props
  "tiled-layer"   -> newComponent uid =<< Layer.newNineTile =<< asSuccess props
  "selection"     -> newComponent uid =<< Selection.new =<< asSuccess props
  _ ->
    throwM $ ComponentCreationFailed $ "Component not defined: `" <> name <> "`"
 where
  asSuccess :: FromJSON a => Value -> MiniLight a
  asSuccess =
    foldResult (throwM . ComponentCreationFailed . T.pack) return . fromJSON

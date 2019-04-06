{-# LANGUAGE DeriveGeneric #-}
module MiniLight.Component.Loader where

import Control.Monad.IO.Class
import Data.Aeson
import qualified Data.HashMap.Strict as HM
import qualified Data.Text as T
import qualified Data.Vector as V
import Data.Yaml (decodeFileEither)
import GHC.Generics
import MiniLight.Light
import MiniLight.Component.Types
import Text.Trifecta

data ComponentConfig = ComponentConfig {
  name :: T.Text,
  properties :: Value
} deriving Generic

instance FromJSON ComponentConfig

data AppConfig = AppConfig {
  app :: [ComponentConfig]
} deriving Generic

instance FromJSON AppConfig

loadAppConfig
  :: FilePath
  -> (T.Text -> Value -> MiniLight Component)
  -> MiniLight [Component]
loadAppConfig path mapper = do
  conf <- liftIO $ either (error . show) id <$> decodeFileEither path
  mapM (\conf -> mapper (name conf) (properties conf)) (app conf)

data Expr
  = Ref T.Text  -- ^ reference syntax: ${ref:...}
  | Var T.Text  -- ^ variable syntax: ${var:...}
  | Op T.Text Expr Expr

parser :: Parser Expr
parser = undefined

resolve :: Value -> Value
resolve = go []
 where
  go path (Object obj) = Object $ HM.mapWithKey (\key -> go (key : path)) obj
  go path (Array  arr) = Array $ V.imap (\i -> go (T.pack (show i) : path)) arr
  go path (String t  ) = String t
  go path (Number n  ) = Number n
  go path (Bool   b  ) = Bool b
  go path Null         = Null

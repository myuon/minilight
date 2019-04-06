{-# LANGUAGE DeriveGeneric #-}
module MiniLight.Component.Loader where

import Control.Applicative
import Control.Monad.IO.Class
import Data.Aeson hiding (Result(..))
import qualified Data.HashMap.Strict as HM
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Vector as V
import Data.Scientific (fromFloatDigits)
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
  = None
  | Ref T.Text  -- ^ reference syntax: ${ref:...}
  | Var T.Text  -- ^ variable syntax: ${var:...}
  | Op T.Text Expr Expr  -- ^ expr operator: +, -, *, /
  | Constant Value  -- ^ constants (string or number or null)
  deriving (Eq, Show)

parser :: Parser Expr
parser = braces (number <|> expr) <|> try reference <|> try variable
 where
  expr      = chainl expr1 op1 None
  expr1     = chainl expr2 op2 None
  expr2     = parens expr <|> try reference <|> try variable <|> try number

  -- low precedence infixl operator group
  op1       = Op "+" <$ textSymbol "+" <|> Op "-" <$ textSymbol "-"

  -- high precedence infixl operator group
  op2       = Op "*" <$ textSymbol "*" <|> Op "/" <$ textSymbol "/"

  reference = do
    char '$'
    braces $ text "ref:" *> (fmap (Ref . T.pack) (many (letter <|> oneOf ".")))
  variable = do
    char '$'
    braces $ text "var:" *> (fmap (Var . T.pack) (many (letter <|> oneOf ".")))
  number = fmap (Constant . Number . either fromIntegral fromFloatDigits)
                integerOrDouble

data Context = Context {
  path :: V.Vector (Either Int T.Text),
  variables :: Object,
  target :: Value
}

getAt :: Value -> [Either Int T.Text] -> Value
getAt = go
 where
  go value        [] = value
  go (Object obj) (Right key:ps) | key `HM.member` obj = go (obj HM.! key) ps
  go (Array  arr) (Left  i  :ps) | 0 <= i && i < V.length arr = go (arr V.! i) ps
  go v (p:_) =
    error
      $  "TypeError: path `"
      <> show p
      <> "` is missing in `"
      <> show v
      <> "`"

normalize
  :: V.Vector (Either Int T.Text) -> [Either Int T.Text] -> [Either Int T.Text]
normalize path1 ts = V.toList path1' ++ dropWhile (\v -> v == Right "") ts
 where
  depth  = length $ takeWhile (\v -> v == Right "") ts
  path1' = V.take (V.length path1 - depth - 1) path1

eval :: Context -> Expr -> Value
eval ctx = go
 where
  go None = ""
  go (Ref path') =
    getAt (target ctx) (normalize (path ctx) (convertPath path'))
  go (Var path') =
    getAt (Object (variables ctx)) (normalize (path ctx) (convertPath path'))

convertPath :: T.Text -> [Either Int T.Text]
convertPath
  = map
      ( \t ->
        foldResult (\_ -> Right t) (Left . fromIntegral) $ parseText index t
      )
    . T.splitOn "."
    . (\t -> if T.length t > 0 && T.head t == '.' then T.tail t else t)
  where index = char '[' *> natural <* char ']'

convert :: Context -> T.Text -> Value
convert ctx = foldResult (error . show) (eval ctx) . parseText parser

parseText :: Parser a -> T.Text -> Result a
parseText parser = parseByteString parser mempty . TE.encodeUtf8

resolve :: Value -> Value
resolve = \value -> go (Context V.empty HM.empty value) value
 where
  go ctx (Object obj)
    | "_vars" `HM.member` obj
    = let vars = obj HM.! "_vars"
      in  go
            ( ctx
              { variables = HM.union ((\(Object o) -> o) $ vars) (variables ctx)
              }
            )
            (Object (HM.delete "_vars" obj))
    | otherwise
    = Object $ HM.mapWithKey
      (\key -> go (ctx { path = V.snoc (path ctx) (Right key) }))
      obj
  go ctx (Array arr) =
    Array $ V.imap (\i -> go (ctx { path = V.snoc (path ctx) (Left i) })) arr
  go ctx (String t) = convert ctx t
  go ctx (Number n) = Number n
  go ctx (Bool   b) = Bool b
  go ctx Null       = Null

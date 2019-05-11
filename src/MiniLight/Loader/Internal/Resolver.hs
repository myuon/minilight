{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ViewPatterns #-}
module MiniLight.Loader.Internal.Resolver where

import Control.Applicative
import Control.Monad
import Data.Aeson hiding (Result)
import qualified Data.HashMap.Strict as HM
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Data.Scientific (fromFloatDigits)
import qualified Data.Vector as V
import MiniLight.Loader.Internal.Types
import Text.Trifecta

data Expr
  = None
  | Ref T.Text  -- ^ reference syntax: ${ref:...}
  | Var T.Text  -- ^ variable syntax: ${var:...}
  | Op T.Text Expr Expr  -- ^ expr operator: +, -, *, /
  | Constant Value  -- ^ constants (string or number or null)
  | Symbol T.Text  -- ^ token symbol
  | App Expr [Expr]  -- ^ function application ($func(a,b,c))
  deriving (Eq, Show)

parser :: Parser Expr
parser = try reference <|> try variable <|> try (char '$' *> braces expr)
 where
  expr  = chainl expr1 op1 None
  expr1 = chainl expr2 op2 None
  expr2 =
    parens expr
      <|> try apply
      <|> try parameter
      <|> try reference
      <|> try variable
      <|> try number
      <|> try strlit

  -- low precedence infixl operator group
  op1       = Op "+" <$ textSymbol "+" <|> Op "-" <$ textSymbol "-"

  -- high precedence infixl operator group
  op2       = Op "*" <$ textSymbol "*" <|> Op "/" <$ textSymbol "/"

  reference = char '$' *> do
    braces $ text "ref:" *> (fmap (Ref . T.pack) (many (letter <|> oneOf ".")))
  variable = char '$' *> do
    braces $ text "var:" *> (fmap (Var . T.pack) (many (letter <|> oneOf ".")))
  number = fmap (Constant . Number . either fromIntegral fromFloatDigits)
                integerOrDouble
  strlit    = fmap (Constant . String) $ stringLiteral

  parameter = char '$' *> do
    fmap (Symbol . T.pack) $ (:) <$> letter <*> many (letter <|> digit)
  apply = do
    func <- parameter
    exps <-
      parens
      $       option []
      $       fmap (filter (/= None))
      $       try
      $       (:)
      <$>     expr
      <*>     expr
      `sepBy` (char ',')
    return $ App func exps

data Context = Context {
  path :: V.Vector (Either Int T.Text),
  variables :: Object
}

getAt :: Value -> [Either Int T.Text] -> Either T.Text Value
getAt = go
 where
  go value        [] = Right value
  go (Object obj) (Right key:ps) | key `HM.member` obj = go (obj HM.! key) ps
  go (Array  arr) (Left  i  :ps) | 0 <= i && i < V.length arr = go (arr V.! i) ps
  go v (p:_) =
    Left
      $  "TypeError: path `"
      <> T.pack (show p)
      <> "` is missing in `"
      <> T.pack (show v)
      <> "`"

normalize
  :: V.Vector (Either Int T.Text) -> [Either Int T.Text] -> [Either Int T.Text]
normalize path1 ts = V.toList path1' ++ dropWhile (\v -> v == Right "") ts
 where
  depth  = length $ takeWhile (\v -> v == Right "") ts
  path1' = V.take (V.length path1 - depth - 1) path1

eval :: Context -> Value -> Expr -> Either T.Text Value
eval ctx target = go
 where
  go None = Right ""
  go (Ref path') =
    either (Left . (("Error in `${ref:" <> path' <> "}`\n") <>)) Right
      $ getAt target (normalize (path ctx) (convertPath path'))
  go (Var path') =
    either (Left . (("Error in `${var:" <> path' <> "}`\n") <>)) Right
      $ getAt (Object (variables ctx)) (normalize V.empty (convertPath path'))
  go (Op "+" e1 e2) = runOp (+) e1 e2
  go (Op "-" e1 e2) = runOp (-) e1 e2
  go (Op "*" e1 e2) = runOp (*) e1 e2
  go (Op "/" e1 e2) = runOp (/) e1 e2
  go expr           = Left $ "Illegal expression: " <> T.pack (show expr)

  runOp op e1 e2 =
    fmap Number
      $   join
      $   (\x y -> op <$> asNumber x <*> asNumber y)
      <$> go e1
      <*> go e2

  asNumber (Number x) = Right x
  asNumber x          = Left $ "Not a number: " <> T.pack (show x)

convertPath :: T.Text -> [Either Int T.Text]
convertPath
  = map
      ( \t ->
        foldResult (\_ -> Right t) (Left . fromIntegral) $ parseText index t
      )
    . T.splitOn "."
    . (\t -> if T.length t > 0 && T.head t == '.' then T.tail t else t)
  where index = char '[' *> natural <* char ']'

convert :: Context -> Value -> T.Text -> Either T.Text Value
convert ctx target t =
  foldResult (\_ -> Right $ String t) (eval ctx target) $ parseText parser t

parseText :: Parser a -> T.Text -> Result a
parseText parser = parseByteString parser mempty . TE.encodeUtf8

resolveWith :: Context -> Value -> Either T.Text Value
resolveWith ctx target = go ctx target
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
    = fmap Object $ sequence $ HM.mapWithKey
      (\key -> go (ctx { path = V.snoc (path ctx) (Right key) }))
      obj
  go ctx (Array arr) = fmap Array $ sequence $ V.imap
    (\i -> go (ctx { path = V.snoc (path ctx) (Left i) }))
    arr
  go ctx (String t) = convert ctx target t
  go _   (Number n) = Right $ Number n
  go _   (Bool   b) = Right $ Bool b
  go _   Null       = Right Null

-- | Interpret a JSON value, and unsafely apply fromRight
resolve :: Value -> Value
resolve = (\(Right a) -> a) . resolveWith (Context V.empty HM.empty)

-- | Create 'AppConfig' value from JSON value
parseAppConfig :: Value -> Either T.Text AppConfig
parseAppConfig = conf (Context V.empty HM.empty)
 where
  conf :: Context -> Value -> Either T.Text AppConfig
  conf ctx (Object obj) | "app" `HM.member` obj =
    let
      ctx' = maybe
        ctx
        ( \vars -> ctx
          { variables = HM.union ((\(Object o) -> o) vars) (variables ctx)
          }
        )
        (HM.lookup "_vars" obj)
    in
      fmap (\v -> AppConfig v V.empty) $ app ctx' (obj HM.! "app")
  conf _ (Object obj) =
    Left $ "path `app` is missing in " <> T.pack (show (Object obj))
  conf _ ast = Left $ "Invalid format: " <> T.pack (show ast)

  app :: Context -> Value -> Either T.Text (V.Vector ComponentConfig)
  app ctx (Array vec) = V.mapM (component ctx) vec
  app _   ast         = Left $ "Invalid format: " <> T.pack (show ast)

  component :: Context -> Value -> Either T.Text ComponentConfig
  component ctx (Object obj) | all (`HM.member` obj) ["name", "properties"] = do
    let
      ctx' = maybe
        ctx
        ( \vars -> ctx
          { variables = HM.union ((\(Object o) -> o) vars) (variables ctx)
          }
        )
        (HM.lookup "_vars" obj)

    nameValue <- resolveWith ctx' (obj HM.! "name")
    case nameValue of
      String name -> do
        props <- resolveWith ctx' (obj HM.! "properties")
        Right $ ComponentConfig name props Nothing
      _ -> Left $ "Invalid format: " <> T.pack (show nameValue)
  component _ ast = Left $ "Invalid format: " <> T.pack (show ast)

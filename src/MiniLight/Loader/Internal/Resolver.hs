{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ViewPatterns #-}
module MiniLight.Loader.Internal.Resolver where

import Control.Applicative
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
    getAt (Object (variables ctx)) (normalize V.empty (convertPath path'))
  go (binds -> Op "+" (Constant (Number n1)) (Constant (Number n2))) =
    Number (n1 + n2)
  go (binds -> Op "-" (Constant (Number n1)) (Constant (Number n2))) =
    Number (n1 - n2)
  go (binds -> Op "*" (Constant (Number n1)) (Constant (Number n2))) =
    Number (n1 * n2)
  go (binds -> Op "/" (Constant (Number n1)) (Constant (Number n2))) =
    Number (n1 / n2)
  go expr = error $ "Illegal expression: " ++ show expr

  binds (Op op e1 e2) = Op op (Constant (eval ctx e1)) (Constant (eval ctx e2))
  binds _             = undefined

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
convert ctx t = foldResult (\_ -> String t) (eval ctx) $ parseText parser t

parseText :: Parser a -> T.Text -> Result a
parseText parser = parseByteString parser mempty . TE.encodeUtf8

resolveWith :: Context -> Value -> Value
resolveWith = go
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
  go _   (Number n) = Number n
  go _   (Bool   b) = Bool b
  go _   Null       = Null

-- | Interpret a JSON value
resolve :: Value -> Value
resolve value = resolveWith (Context V.empty HM.empty value) value

-- | AST for the current syntax is just a JSON value.
type AST = Value

-- | Create 'AppConfig' value from AST
evaluate :: AST -> Either T.Text AppConfig
evaluate value = conf (Context V.empty HM.empty value) value
 where
  conf :: Context -> AST -> Either T.Text AppConfig
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
    Left $ "`app` Not Found: " <> T.pack (show (Object obj))
  conf _ ast = Left $ "Invalid format: " <> T.pack (show ast)

  app :: Context -> AST -> Either T.Text (V.Vector ComponentConfig)
  app ctx (Array vec) = V.mapM (component ctx) vec
  app _   ast         = Left $ "Invalid format: " <> T.pack (show ast)

  component :: Context -> AST -> Either T.Text ComponentConfig
  component ctx (Object obj) | all (`HM.member` obj) ["name", "properties"] =
    let
      ctx' = maybe
        ctx
        ( \vars -> ctx
          { variables = HM.union ((\(Object o) -> o) vars) (variables ctx)
          }
        )
        (HM.lookup "_vars" obj)
      String name = resolveWith ctx' (obj HM.! "name")
      props       = resolveWith ctx' (obj HM.! "properties")
    in
      Right $ ComponentConfig name props HM.empty
  component _ ast = Left $ "Invalid format: " <> T.pack (show ast)

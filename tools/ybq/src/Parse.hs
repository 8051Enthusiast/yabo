module Parse (Parse.parse) where

import Builtin (ybqBuiltins)
import Control.Monad.Reader (runReaderT)
import Data.Maybe (fromMaybe)
import Expr
import Ops (PrimOps)
import Text.Parsec
import Text.Parsec.Expr
import Text.Parsec.Language
import Text.Parsec.Token
import ValGen (ValGen)

type Parser a = ParsecT String () IO a

langDef :: GenLanguageDef String () IO
langDef =
  emptyDef
    { commentStart = "",
      commentEnd = "",
      commentLine = "#",
      nestedComments = False,
      identStart = letter <|> char '_',
      identLetter = alphaNum <|> char '_',
      opStart = oneOf "+-*/%<>=!&|?",
      opLetter = oneOf "+-*/%<>=!&|?",
      reservedOpNames = ["+", "-", "*", "/", "%", ".", "<", ">", "=", "<=", ">=", "==", "!=", "?", "//", ".", "|"],
      reservedNames = ["as", "def", "if", "then", "else", "elif", "end", "and", "or", "try", "catch", "label", "break", "reduce"],
      caseSensitive = True
    }

l :: GenTokenParser String () IO
l = makeTokenParser langDef

parse :: (PrimOps a) => String -> SourceName -> IO (Either ParseError (a -> ValGen a))
parse s source = do
  parsed <- runParserT program () source s
  builtin <- builtins
  return $ case (parsed, builtin) of
    (Left err, _) -> Left err
    (_, Left err) -> Left err
    (Right f, Right b) -> Right $ \x -> runReaderT (b f x) emptyEnv

builtins :: (PrimOps a) => IO (Either ParseError (Expr a -> Expr a))
builtins = runParserT defs () "builtins" ybqBuiltins

type Op a = Operator String () IO a

program :: (PrimOps a) => Parser (Expr a)
program = whiteSpace l >> topExpr <* eof

argName :: Parser (String, Bool)
argName = (,True) <$> binding <|> (,False) <$> identifier l

topExpr :: (PrimOps a) => Parser (Expr a)
topExpr = do
  df <- defs
  df <$> ctrlExpr

defs :: (PrimOps a) => Parser (Expr a -> Expr a)
defs = do
  ds <- many (def <* semi l)
  return $ foldr (.) id ds

def :: (PrimOps a) => Parser (Expr a -> Expr a)
def = do
  reserved l "def"
  name <- identifier l
  args <- parens l (sepBy argName $ semi l) <|> pure []
  _ <- colon l
  defExpr name args <$> topExpr

prefix :: String -> (a -> a) -> Op a
prefix op f = Prefix (reservedOp l op >> return f)

infx :: String -> (a -> a -> a) -> Assoc -> Op a
infx op f = Infix (reservedOp l op >> return f)

infxIdent :: String -> (a -> a -> a) -> Assoc -> Op a
infxIdent op f = Infix (reserved l op >> return f)

ctrlExpr :: (PrimOps a) => Parser (Expr a)
ctrlExpr =
  labeled <|> reduceTerm <|> do
    lhs <- expr
    as <- optionMaybe (reserved l "as" >> binding)
    bar <- optionMaybe $ reservedOp l "|"
    case (as, bar) of
      (Just name, Just _) -> bindExpr name lhs <$> ctrlExpr
      (Nothing, Just _) -> seqExpr lhs <$> ctrlExpr
      _ -> return lhs

labeled :: (PrimOps a) => Parser (Expr a)
labeled = do
  reserved l "label"
  lbl <- binding
  reservedOp l "|"
  labelExpr lbl <$> ctrlExpr

reduceTerm :: (PrimOps a) => Parser (Expr a)
reduceTerm = do
  reserved l "reduce"
  from <- term
  _ <- reserved l "as"
  name <- binding
  exprs <- parens l $ sepBy topExpr (semi l)
  case exprs of
    [initExpr, step] -> return $ reduceExpr name from initExpr step
    _ -> fail "Invalid reduce expression"

expr :: (PrimOps a) => Parser (Expr a)
expr = buildExpressionParser table term
  where
    table =
      [ [prefix "-" negExpr],
        [infx "*" mulExpr AssocLeft, infx "/" divExpr AssocLeft, infx "%" modExpr AssocLeft],
        [infx "+" addExpr AssocLeft, infx "-" subExpr AssocLeft],
        [ infx "==" eqExpr AssocNone,
          infx "!=" neqExpr AssocNone,
          infx "<" ltExpr AssocNone,
          infx "<=" leqExpr AssocNone,
          infx ">" gtExpr AssocNone,
          infx ">=" geqExpr AssocNone
        ],
        [infxIdent "and" andExpr AssocLeft],
        [infxIdent "or" orExpr AssocLeft],
        [infx "//" coalExpr AssocRight],
        [infx "," catExpr AssocLeft]
      ]

exprD :: (PrimOps a) => Parser (Expr a)
exprD = buildExpressionParser table term
  where
    table = [[prefix "-" negExpr], [infx "|" seqExpr AssocRight]]

identOrKeyword :: Parser String
identOrKeyword = lexeme l $ do
  first <- letter <|> char '_'
  rest <- many (alphaNum <|> char '_')
  return $ first : rest

binding :: Parser String
binding = char '$' *> identOrKeyword

dictTerm :: (PrimOps a) => Parser (Expr a)
dictTerm = braces l $ do
  stmts <- sepBy dictPair (comma l)
  return $ dictExpr stmts

dictPair :: (PrimOps a) => Parser (Expr a, Expr a)
dictPair = constKeyPair <|> exprKeyPair

constKeyPair :: (PrimOps a) => Parser (Expr a, Expr a)
constKeyPair = do
  k <- stringLiteral l <|> identOrKeyword
  t <- dictTail
  case t of
    Just v -> return (strExpr k, v)
    Nothing -> return (strExpr k, indexExpr $ strExpr k)

exprKeyPair :: (PrimOps a) => Parser (Expr a, Expr a)
exprKeyPair = do
  k <- parens l topExpr
  _ <- colon l
  (k,) <$> exprD

dictTail :: (PrimOps a) => Parser (Maybe (Expr a))
dictTail = (do _ <- colon l; Just <$> exprD) <|> pure Nothing

term :: (PrimOps a) => Parser (Expr a)
term =
  tryTerm
    <|> (reserved l "if" >> ifTermTail)
    <|> dotAtom
    <|> do
      t <- atom
      (seqExpr t <$> tailTerm) <|> pure t

ifTermTail :: (PrimOps a) => Parser (Expr a)
ifTermTail = do
  cond <- topExpr
  reserved l "then"
  t <- topExpr
  ( do
      reserved l "else"
      f <- topExpr
      reserved l "end"
      return $ ifExpr cond t f
    )
    <|> ( do
            reserved l "elif"
            ifExpr cond t <$> ifTermTail
        )

tryTerm :: (PrimOps a) => Parser (Expr a)
tryTerm = do
  reserved l "try"
  t <- term
  catch <- optionMaybe $ reserved l "catch"
  case catch of
    Just _ -> tryCatchExpr t <$> term
    Nothing -> return $ tryExpr t

dotAtom :: (PrimOps a) => Parser (Expr a)
dotAtom = do
  _ <- dot l
  idx <- optionMaybe dotTail
  case idx of
    Just i -> seqExpr i <$> tailTerm
    Nothing -> pure pure

atom :: (PrimOps a) => Parser (Expr a)
atom =
  parens l topExpr
    <|> dictTerm
    <|> do t <- brackets l $ optionMaybe topExpr; pure $ arrExpr $ fromMaybe emptyExpr t
    <|> (intExpr <$> natural l)
    <|> (strExpr <$> stringLiteral l)
    <|> (varExpr <$> binding)
    <|> brkTerm
    <|> call

brkTerm :: Parser (Expr a)
brkTerm = reserved l "break" >> breakExpr <$> binding

call :: (PrimOps a) => Parser (Expr a)
call = do
  f <- identifier l
  args <- parens l (sepBy topExpr (semi l)) <|> pure []
  return $ case (f, args) of
    ("type", []) -> typeExpr
    ("tostring", []) -> toStringExpr
    ("null", []) -> nullExpr
    ("true", []) -> boolExpr True
    ("false", []) -> boolExpr False
    ("empty", []) -> emptyExpr
    ("error", []) -> errorExpr
    _ -> callExpr f args

tailTerm :: (PrimOps a) => Parser (Expr a)
tailTerm = do
  subterms <- many (bracketTail <|> (dot l >> dotTail))
  return $ foldr seqExpr idExpr subterms

dotTail :: (PrimOps a) => Parser (Expr a)
dotTail =
  (do name <- identifier l; pure $ indexExpr $ strExpr name)
    <|> bracketTail

bracketTail :: (PrimOps a) => Parser (Expr a)
bracketTail = do
  index <- brackets l $ optionMaybe topExpr
  let res = maybe iterExpr indexExpr index
  q <- optionMaybe $ reserved l "?"
  case q of
    Just _ -> pure $ tryExpr res
    Nothing -> pure res
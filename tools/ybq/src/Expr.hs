module Expr (module Expr) where

import Control.Monad.Reader (MonadReader (ask), ReaderT (ReaderT, runReaderT))
import Control.Monad.Trans (lift)
import Data.Map (Map, fromList, insert, lookup)
import Data.Maybe (fromMaybe)
import Ops (PrimOps (..))
import ValGen (Result (BreakResult, ErrorResult, Result), ValGen (Break, Cons, End, Error), catchBreak, catchError, fromResult, lastVal)
import YaboVal (YaboVal (YaboArray, YaboBit, YaboBlock, YaboInt, YaboNull, YaboString))
import Prelude hiding (iterate)

data BreakMap = BreakMap {breaks :: Map String Int, current :: Int}

insertBreak :: String -> BreakMap -> (BreakMap, Int)
insertBreak name bm =
  ( bm {breaks = insert name (current bm) (breaks bm), current = current bm + 1},
    current bm
  )

emptyBreakMap :: BreakMap
emptyBreakMap = BreakMap mempty 0

data Env v = Env {vars :: Map String v, funcs :: Map (String, Int) ([Expr v] -> Expr v), brk :: BreakMap}

insertVar :: String -> v -> Env v -> Env v
insertVar name value env = env {vars = Data.Map.insert name value (vars env)}

insertFunc :: String -> Int -> ([Expr v] -> Expr v) -> Env v -> Env v
insertFunc name arity value env = env {funcs = Data.Map.insert (name, arity) value (funcs env)}

insertEnvBrk :: String -> Env v -> (Env v, Int)
insertEnvBrk name env = (env {brk = env'}, i)
  where
    (env', i) = insertBreak name (brk env)

lookupBreak :: String -> Env v -> Maybe Int
lookupBreak name env = Data.Map.lookup name (breaks $ brk env)

type Res e v = ReaderT (Env e) ValGen v

type Expr v = v -> Res v v

type Tertiary a = (PrimOps a) => Expr a -> Expr a -> Expr a -> Expr a

type Binary a = (PrimOps a) => Expr a -> Expr a -> Expr a

type Unary a = (PrimOps a) => Expr a -> Expr a

lifted :: Result a -> Res a a
lifted = lift . fromResult

binOp :: (v -> v -> Result v) -> Binary v
binOp op a b v = do
  b' <- b v
  a' <- a v
  lifted $ op a' b'

unOp :: (v -> Result v) -> Unary v
unOp op a v = do
  a' <- a v
  lifted $ op a'

addExpr, subExpr, mulExpr, divExpr, modExpr :: Binary v
addExpr = binOp add
subExpr = binOp sub
mulExpr = binOp mul
divExpr = binOp Ops.div
modExpr = binOp Ops.mod

negExpr, indexExpr :: Unary v
negExpr = unOp neg
indexExpr a v = do
  idx <- a v
  lifted $ Ops.index v idx

notExpr :: (PrimOps v) => Expr v
notExpr = lifted . Ops.not

seqExpr :: Binary v
seqExpr a b v = do
  r <- a v
  b r

catExpr :: Binary v
catExpr a b v = do
  let a' = a v
  let b' = b v
  ReaderT $ \env -> runReaderT a' env <> runReaderT b' env

iterExpr :: (PrimOps v) => Expr v
iterExpr v = lift $ Ops.iter v

varExpr :: String -> Expr v
varExpr name _ = do
  env <- ask
  lift
    $ fromResult
    $ maybe
      (ErrorResult $ "Variable " ++ name ++ " not found")
      Result
    $ Data.Map.lookup name (vars env)

litExpr :: (PrimOps v) => YaboVal -> Expr v
litExpr x _ = lifted $ val x

intExpr :: (PrimOps v) => Integer -> Expr v
intExpr x = litExpr $ YaboInt x

strExpr :: (PrimOps v) => String -> Expr v
strExpr x = litExpr $ YaboString x

boolExpr :: (PrimOps v) => Bool -> Expr v
boolExpr x = litExpr $ YaboBit x

nullExpr :: (PrimOps v) => Expr v
nullExpr = litExpr YaboNull

idExpr :: Expr v
idExpr = pure

emptyExpr :: Expr v
emptyExpr _ = lift End

breakExpr :: String -> Expr v
breakExpr label _ = do
  env <- ask
  case lookupBreak label env of
    Just i -> lift $ Break i
    Nothing -> lift $ Error $ "Break label " ++ label ++ " not found"

errorExpr :: (PrimOps v) => Expr v
errorExpr v = case toVal v of
  YaboString s -> lift $ Error s
  _ -> lift $ Error "Error, not a string"

labelExpr :: String -> Unary v
labelExpr label cont v = do
  env <- ask
  let (env', i) = insertEnvBrk label env
  lift $ catchBreak i $ runReaderT (cont v) env'

bindExpr :: String -> Binary v
bindExpr name a b v = do
  a' <- a v
  env <- ask
  let env' = insertVar name a' env
  lift $ runReaderT (b v) env'

typeExpr :: (PrimOps v) => Expr v
typeExpr a = lifted $ val $ YaboString $ typeOf a

toStringExpr :: (PrimOps v) => Expr v
toStringExpr a = lifted $ val $ YaboString $ toString a

ifExpr :: Tertiary v
ifExpr cond a b v = do
  cond' <- cond v
  if truthy cond'
    then a v
    else b v

tryExpr :: Unary v
tryExpr a v = ReaderT $ catchError . runReaderT (a v)

tryCatchExpr :: Binary v
tryCatchExpr a = catExpr (tryExpr a)

eqExpr, neqExpr, leqExpr, geqExpr, ltExpr, gtExpr :: Binary v
eqExpr = binOp (\a b -> val $ YaboBit $ equals a b)
neqExpr = binOp (\a b -> val $ YaboBit $ Prelude.not $ equals a b)
leqExpr = binOp (\a b -> val $ YaboBit $ order a b /= GT)
geqExpr = binOp (\a b -> val $ YaboBit $ order a b /= LT)
ltExpr = binOp (\a b -> val $ YaboBit $ order a b == LT)
gtExpr = binOp (\a b -> val $ YaboBit $ order a b == GT)

intoTruthy :: Unary v
intoTruthy = unOp (val . YaboBit . truthy)

andExpr, orExpr :: Binary v
andExpr a b = ifExpr a (intoTruthy b) (litExpr $ YaboBit False)
orExpr a b = ifExpr a (litExpr $ YaboBit True) (intoTruthy b)

arrExpr :: Unary a
arrExpr a v = do
  ReaderT $ \env -> case intoSeq $ runReaderT (a v) env of
    Result x -> fromResult $ val $ YaboArray x
    ErrorResult s -> Error s
    BreakResult s -> Break s

coalExpr :: Binary a
coalExpr a b v = do
  a' <- a v
  if truthy a'
    then return a'
    else b v

exprToVal :: (PrimOps a) => Expr a -> a -> Res a YaboVal
exprToVal a v = do
  a' <- a v
  return $ toVal a'

toStr :: (PrimOps a) => Expr a -> a -> Res a String
toStr a v = do
  a' <- a v
  case toVal a' of
    YaboString s -> return s
    _ -> lift $ Error "Expected string in field name"

dictExpr :: (PrimOps a) => [(Expr a, Expr a)] -> Expr a
dictExpr kvs r = do
  let kvs' = fmap (\(k, v) -> (toStr k r, exprToVal v r)) kvs
  kvs'' <- traverse (\(k, v) -> (,) <$> k <*> v) kvs'
  lifted $ val $ YaboBlock $ fromList kvs''

lengthExpr :: (PrimOps a) => Expr a
lengthExpr a = lifted $ Ops.length a

keysExpr :: (PrimOps a) => Expr a
keysExpr a = lifted $ Ops.keys a

captured :: Env a -> Unary a
captured e a v = ReaderT $ \_ -> runReaderT (a v) e

withArgInEnv :: ((String, Bool), Expr a) -> Expr a -> Expr a
withArgInEnv ((name, value), arg) cont v = do
  env <- ask
  let env' = insertFunc name 0 (const arg) env
  newEnv <-
    if value
      then do
        arg' <- arg v
        return $ insertVar name arg' env'
      else do
        return env'
  lift $ runReaderT (cont v) newEnv

definition :: [(String, Bool)] -> Expr a -> [Expr a] -> Expr a
definition argNames body args v = do
  let argNames' = zip argNames args
  let body' = foldr withArgInEnv body argNames'
  body' v

defExpr :: String -> [(String, Bool)] -> Expr a -> Expr a -> Expr a
defExpr name args body cont v = do
  env <- ask
  let arity = Prelude.length args
  let func = definition args body
  let newEnv = insertFunc name arity func env
  lift $ runReaderT (cont v) newEnv

callExpr :: (PrimOps a) => String -> [Expr a] -> Expr a
callExpr name args v = do
  env <- ask
  let arity = Prelude.length args
  let func = Data.Map.lookup (name, arity) (funcs env)
  let args' = fmap (captured env) args
  case func of
    Just f -> lift $ runReaderT (f args' v) env
    Nothing -> lift $ Error $ "Function " ++ name ++ "/" ++ show arity ++ " not found"

reduceStep :: (PrimOps a) => a -> a -> String -> Env a -> Expr a -> Result a
reduceStep acc item name env expr = withEnv $ expr acc
  where
    withEnv expr' = fromMaybe (val YaboNull) $ lastVal $ runReaderT expr' (insertVar name item env)

runReduce :: (PrimOps a) => ValGen a -> a -> String -> Env a -> Expr a -> Result a
runReduce (Error s) _ _ _ _ = ErrorResult s
runReduce (Break i) _ _ _ _ = BreakResult i
runReduce End acc _ _ _ = Result acc
runReduce (Cons item vg) acc name env expr =
  case reduceStep acc item name env expr of
    Result acc' -> seq acc' $ runReduce vg acc' name env expr
    other -> other

reduceExpr :: (PrimOps a) => String -> Tertiary a
reduceExpr name from initial step v = do
  init' <- initial v
  env <- ask
  let arr = runReaderT (from v) env
  lifted $ runReduce arr init' name env step

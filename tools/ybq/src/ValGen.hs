module ValGen (ValGen (..), Result (..), catchBreak, catchError, toSeq, fromFoldable, fromResult, lastVal) where

import Data.Sequence (Seq, (|>))

data ValGen v
  = Cons v (ValGen v)
  | End
  | Error String
  | Break Int

instance Functor ValGen where
  fmap f (Cons v vg) = Cons (f v) (fmap f vg)
  fmap _ End = End
  fmap _ (Error s) = Error s
  fmap _ (Break s) = Break s

instance Semigroup (ValGen v) where
  End <> vg = vg
  Cons v vg <> vg' = Cons v (vg <> vg')
  Error s <> _ = Error s
  Break s <> _ = Break s

instance Monoid (ValGen v) where
  mempty = End

instance Applicative ValGen where
  pure x = Cons x End
  Cons f fg <*> vg = fmap f vg <> (fg <*> vg)
  End <*> _ = End
  Error s <*> _ = Error s
  Break s <*> _ = Break s

instance Monad ValGen where
  Cons v vg >>= f = f v <> (vg >>= f)
  End >>= _ = End
  Error s >>= _ = Error s
  Break s >>= _ = Break s

instance (Show v) => Show (ValGen v) where
  show (Cons v End) = show v
  show (Cons v vg) = show v ++ "\n" ++ show vg
  show End = ""
  show (Error s) = "Error: " ++ s
  show (Break s) = "Break: " ++ show s

instance Foldable ValGen where
  foldr f z (Cons v vg) = f v (foldr f z vg)
  foldr _ z End = z
  foldr _ z (Error _) = z
  foldr _ z (Break _) = z

instance Traversable ValGen where
  traverse f (Cons v vg) = Cons <$> f v <*> traverse f vg
  traverse _ End = pure End
  traverse _ (Error s) = pure $ Error s
  traverse _ (Break s) = pure $ Break s

catchBreak :: Int -> ValGen v -> ValGen v
catchBreak _ End = End
catchBreak _ (Error s) = Error s
catchBreak s (Break r) = if s == r then End else Break r
catchBreak s (Cons v vg) = Cons v (catchBreak s vg)

catchError :: ValGen v -> ValGen v
catchError End = End
catchError (Break s) = Break s
catchError (Cons v vg) = Cons v (catchError vg)
catchError (Error _) = End

fromFoldable :: (Foldable f) => f v -> ValGen v
fromFoldable = foldr Cons End

data Result v
  = Result v
  | ErrorResult String
  | BreakResult Int

lastVal :: ValGen v -> Maybe (Result v)
lastVal (Cons v End) = Just $ Result v
lastVal (Cons _ vg) = lastVal vg
lastVal (Error s) = Just $ ErrorResult s
lastVal (Break s) = Just $ BreakResult s
lastVal End = Nothing

toSeq :: ValGen v -> Result (Seq v)
toSeq =
  let go acc End = Result acc
      go _ (Error s) = ErrorResult s
      go _ (Break s) = BreakResult s
      go acc (Cons x xs) = go (acc |> x) xs
   in go mempty

fromResult :: Result v -> ValGen v
fromResult (Result v) = Cons v End
fromResult (ErrorResult s) = Error s
fromResult (BreakResult s) = Break s

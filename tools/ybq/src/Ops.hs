module Ops (PrimOps (..)) where

import Data.Map (elemAt)
import Data.Map qualified
import Data.Maybe qualified
import Data.Sequence (Seq, cycleTaking, drop, fromFunction, length, lookup, take)
import ValGen
  ( Result (ErrorResult, Result),
    ValGen,
    fromFoldable,
    fromResult,
    toSeq,
  )
import YaboVal (YaboVal (..))

class PrimOps a where
  slice :: a -> a -> a -> Result a
  add, sub, mul, div, mod, index :: a -> a -> Result a
  neg, not, keys, length :: a -> Result a
  toVal :: a -> YaboVal
  val :: YaboVal -> Result a
  iter :: a -> ValGen a
  typeOf :: a -> String
  toString :: a -> String
  truthy :: a -> Bool
  equals :: a -> a -> Bool
  order :: a -> a -> Ordering
  intoSeq :: ValGen a -> Result (Seq YaboVal)

toInt :: Integer -> Maybe Int
toInt x = if x > toInteger (maxBound :: Int) || x < toInteger (minBound :: Int) then Nothing else Just $ fromInteger x

cappedIndex :: Int -> Integer -> Int
cappedIndex len = saturated . absolute
  where
    saturated idx
      | idx < 0 = 0
      | idx > toInteger len = len
      | otherwise = fromInteger idx
    absolute idx
      | idx < 0 = toInteger len + idx
      | otherwise = idx

instance PrimOps YaboVal where
  add (YaboInt a) (YaboInt b) = Result $ YaboInt $ a + b
  add (YaboChar a) (YaboChar b) = Result $ YaboString [a, b]
  add (YaboString a) (YaboString b) = Result $ YaboString $ a ++ b
  add (YaboChar a) (YaboString b) = Result $ YaboString $ a : b
  add (YaboString a) (YaboChar b) = Result $ YaboString $ a ++ [b]
  add (YaboArray a) (YaboArray b) = Result $ YaboArray $ a <> b
  add (YaboBlock a) (YaboBlock b) = Result $ YaboBlock $ a <> b
  add YaboNull a = Result a
  add a YaboNull = Result a
  add a b = ErrorResult $ "Invalid addition of " ++ typeOf a ++ " and " ++ typeOf b

  sub (YaboInt a) (YaboInt b) = Result $ YaboInt $ a - b
  sub a b = ErrorResult $ "Invalid subtraction of " ++ typeOf a ++ " and " ++ typeOf b

  mul (YaboInt a) (YaboInt b) = Result $ YaboInt $ a * b
  mul (YaboArray a) (YaboInt b) =
    case toInt $ toInteger (Data.Sequence.length a) * b of
      Nothing -> ErrorResult "Array too large"
      Just _ -> Result $ YaboArray $ Data.Sequence.cycleTaking (fromInteger b) a
  mul (YaboString a) (YaboInt b) = Result $ YaboString $ concat $ Prelude.replicate (fromInteger b) a
  mul a b = ErrorResult $ "Invalid multiplication of " ++ typeOf a ++ " and " ++ typeOf b

  div (YaboInt _) (YaboInt 0) = ErrorResult "Division by zero"
  div (YaboInt a) (YaboInt b) = Result $ YaboInt $ a `Prelude.div` b
  div a b = ErrorResult $ "Invalid division of " ++ typeOf a ++ " and " ++ typeOf b

  mod (YaboInt a) (YaboInt 0) = Result $ YaboInt a
  mod (YaboInt a) (YaboInt b) = Result $ YaboInt $ a `Prelude.mod` b
  mod a b = ErrorResult $ "Invalid modulo of " ++ typeOf a ++ " and " ++ typeOf b

  index (YaboArray a) (YaboInt b) = case toInt b of
    Nothing -> Result YaboNull
    Just b' -> Result $ Data.Maybe.fromMaybe YaboNull (Data.Sequence.lookup idx a)
      where
        idx = if b' < 0 then Data.Sequence.length a + b' else b'
  index (YaboBlock a) (YaboString b) = Result $ Data.Maybe.fromMaybe YaboNull (Data.Map.lookup b a)
  index YaboNull _ = Result YaboNull
  index a b = ErrorResult $ "Invalid indexing of " ++ typeOf a ++ " and " ++ typeOf b

  slice (YaboArray a) (YaboInt start) (YaboInt end) =
    Result $
      YaboArray $
        Data.Sequence.take (end_idx - start_idx) $
          Data.Sequence.drop start_idx a
    where
      start_idx = conv start
      end_idx = conv end
      conv = cappedIndex $ Data.Sequence.length a
  slice (YaboString a) (YaboInt start) (YaboInt end) =
    Result $ YaboString $ Prelude.take (end_idx - start_idx) $ Prelude.drop start_idx a
    where
      start_idx = conv start
      end_idx = conv end
      conv = cappedIndex $ Prelude.length a
  slice arr YaboNull other = slice arr (YaboInt 0) other
  slice (YaboArray a) other YaboNull = slice (YaboArray a) other (YaboInt $ toInteger $ Data.Sequence.length a)
  slice (YaboString a) other YaboNull = slice (YaboString a) other (YaboInt $ toInteger $ Prelude.length a)
  slice a b c = ErrorResult $ "Invalid slicing of " ++ typeOf a ++ " with " ++ typeOf b ++ " and " ++ typeOf c

  neg (YaboInt a) = Result $ YaboInt $ -a
  neg a = ErrorResult $ "Invalid negation of " ++ typeOf a

  not a = Result $ YaboBit $ Prelude.not $ truthy a

  keys (YaboArray a) = Result $ YaboArray $ fromFunction (Data.Sequence.length a) (YaboInt . toInteger)
  keys (YaboBlock a) = Result $ YaboArray $ fromFunction (Data.Map.size a) (YaboString . fst . flip elemAt a)
  keys a = ErrorResult $ "Cannot get keys of " ++ typeOf a

  length (YaboArray a) = Result $ YaboInt $ toInteger $ Data.Sequence.length a
  length (YaboBlock a) = Result $ YaboInt $ toInteger $ Data.Map.size a
  length (YaboInt a) = Result $ YaboInt $ abs a
  length (YaboString a) = Result $ YaboInt $ toInteger $ Prelude.length a
  length YaboNull = Result $ YaboInt 0
  length a = ErrorResult $ "Cannot get length of " ++ typeOf a

  toVal = id

  val = Result

  iter (YaboArray a) = fromFoldable a
  iter (YaboBlock a) = fromFoldable $ snd <$> Data.Map.toList a
  iter a = fromResult $ ErrorResult $ "Cannot iterate over " ++ typeOf a

  typeOf (YaboInt _) = "number"
  typeOf (YaboChar _) = "char"
  typeOf (YaboBit _) = "boolean"
  typeOf (YaboError _) = "error"
  typeOf (YaboArray _) = "array"
  typeOf (YaboBlock _) = "object"
  typeOf (YaboString _) = "string"
  typeOf YaboUnit = "unit"
  typeOf YaboParser = "parser"
  typeOf YaboFunction = "function"
  typeOf YaboNull = "null"

  toString (YaboString a) = show a
  toString a = Ops.toString a

  truthy (YaboBit False) = False
  truthy YaboNull = False
  truthy (YaboError _) = False
  truthy _ = True

  equals = (==)

  order = compare

  intoSeq = toSeq
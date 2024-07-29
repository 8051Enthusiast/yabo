module Ops (PrimOps (..)) where

import Data.Map qualified
import Data.Maybe qualified
import Data.Sequence (Seq, cycleTaking, length, lookup)
import ValGen
  ( Result (ErrorResult, Result),
    ValGen,
    fromFoldable,
    fromResult,
    toSeq,
  )
import YaboVal (YaboVal (..))

class PrimOps a where
  add, sub, mul, div, mod, index :: a -> a -> Result a
  neg, not :: a -> Result a
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

  neg (YaboInt a) = Result $ YaboInt $ -a
  neg a = ErrorResult $ "Invalid negation of " ++ typeOf a

  not (YaboBit a) = Result $ YaboBit $ Prelude.not a
  not a = ErrorResult $ "Invalid negation of " ++ typeOf a

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
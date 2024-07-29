{-# LANGUAGE CApiFFI #-}

module YaboBindings
  ( ybqTypeFromCInt,
    ybqStatusFromWord64,
    YbqType (..),
    YbqReturnStatus (..),
    FieldName (..),
    fieldNameToString,
    ybqCallInit,
    ybqType,
    ybqAllocSize,
    ybqParseBytes,
    ybqFieldAccess,
    ybqFieldNameAtIndex,
    ybqFieldCount,
    ybqArraySize,
    ybqArrayAccess,
    ybqGetInt,
    ybqGetChar,
    ybqGetBit,
    ybqGetError,
  )
where

import Foreign
import Foreign.C.String
import Foreign.C.Types

data YbqType
  = YbqTypeInt
  | YbqTypeChar
  | YbqTypeBit
  | YbqTypeError
  | YbqTypeArray
  | YbqTypeBlock
  | YbqTypeUnit
  | YbqTypeParser
  | YbqTypeFunction
  deriving (Eq, Show)

ybqTypeFromCInt :: CInt -> YbqType
ybqTypeFromCInt 0 = YbqTypeInt
ybqTypeFromCInt 1 = YbqTypeChar
ybqTypeFromCInt 2 = YbqTypeBit
ybqTypeFromCInt 3 = YbqTypeError
ybqTypeFromCInt 4 = YbqTypeArray
ybqTypeFromCInt 5 = YbqTypeBlock
ybqTypeFromCInt 6 = YbqTypeUnit
ybqTypeFromCInt 7 = YbqTypeParser
ybqTypeFromCInt 8 = YbqTypeFunction
ybqTypeFromCInt _ = error "Unknown YaboType"

data YbqReturnStatus
  = YbqStatusOk
  | YbqStatusError
  | YbqStatusEos
  | YbqStatusBacktrack
  deriving (Eq, Ord, Show)

ybqStatusFromWord64 :: Word64 -> YbqReturnStatus
ybqStatusFromWord64 0 = YbqStatusOk
ybqStatusFromWord64 1 = YbqStatusError
ybqStatusFromWord64 2 = YbqStatusEos
ybqStatusFromWord64 3 = YbqStatusBacktrack
ybqStatusFromWord64 _ = error "Unknown YaboReturnStatus"

newtype FieldName = FieldName CString deriving (Eq, Show, Ord)

fieldNameToString :: FieldName -> IO String
fieldNameToString (FieldName cstr) = peekCString cstr

foreign import capi "yabo-bindings.h ybq_call_init" ybqCallInit :: Ptr () -> CSize -> Ptr () -> IO Word64

foreign import capi "yabo-bindings.h ybq_type" ybqType :: Ptr () -> IO CInt

foreign import capi "yabo-bindings.h ybq_alloc_size" ybqAllocSize :: Ptr () -> IO CSize

foreign import capi "yabo-bindings.h ybq_parse_bytes" ybqParseBytes :: Ptr () -> Ptr () -> CSize -> Ptr () -> IO Word64

foreign import capi "yabo-bindings.h ybq_field_access" ybqFieldAccess :: Ptr () -> Ptr () -> CSize -> IO Word64

foreign import capi "yabo-bindings.h ybq_field_name_at_index" ybqFieldNameAtIndex :: Ptr () -> CSize -> IO FieldName

foreign import capi "yabo-bindings.h ybq_field_count" ybqFieldCount :: Ptr () -> IO CSize

foreign import capi "yabo-bindings.h ybq_array_size" ybqArraySize :: Ptr () -> IO CSize

foreign import capi "yabo-bindings.h ybq_array_access" ybqArrayAccess :: Ptr () -> Ptr () -> CSize -> IO Word64

foreign import capi "yabo-bindings.h ybq_int" ybqGetInt :: Ptr () -> IO Word64

foreign import capi "yabo-bindings.h ybq_char" ybqGetChar :: Ptr () -> IO Word32

foreign import capi "yabo-bindings.h ybq_bit" ybqGetBit :: Ptr () -> IO Word8

foreign import capi "yabo-bindings.h ybq_error" ybqGetError :: Ptr () -> IO Word64
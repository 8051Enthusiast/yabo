module YaboVal (YaboVal (..), getParser, parseWithArgs, openLibrary, toString) where

import Control.Monad.Identity (Identity (Identity), runIdentity)
import Control.Monad.Trans (liftIO)
import Control.Monad.Trans.Maybe (MaybeT (MaybeT, runMaybeT))
import Data.ByteString.Short (ShortByteString, packCStringLen, useAsCStringLen)
import Data.Foldable (toList)
import Data.List (intercalate)
import Data.Map (Map, fromList, toList)
import Data.Maybe (catMaybes)
import Data.Sequence (Seq, fromFunction)
import Foreign (FunPtr, Ptr, allocaBytes, castFunPtrToPtr, castPtr, nullFunPtr, plusForeignPtr, touchForeignPtr, withForeignPtr)
import Foreign.Storable (Storable(..), peek, sizeOf)
import Foreign.Marshal.Array (pokeArray)
import Foreign.C (CChar, CSize)
import Foreign.ForeignPtr (ForeignPtr)
import Data.Int (Int64)
import GHC.IO (unsafePerformIO)
import System.Posix.DynamicLinker qualified as DL
import YaboBindings
  ( YbqReturnStatus (YbqStatusBacktrack, YbqStatusEos, YbqStatusOk),
    YbqType
      ( YbqTypeArray,
        YbqTypeBit,
        YbqTypeBlock,
        YbqTypeChar,
        YbqTypeError,
        YbqTypeFunction,
        YbqTypeInt,
        YbqTypeParser,
        YbqTypeUnit
      ),
    fieldNameToString,
    ybqAllocSize,
    ybqArrayAccess,
    ybqArraySize,
    ybqCallInit,
    ybqExportArgCount,
    ybqFieldAccess,
    ybqFieldCount,
    ybqFieldNameAtIndex,
    ybqGetError,
    ybqGetInt,
    ybqParseBytesWithArgs,
    ybqStatusFromWord64,
    ybqType,
    ybqTypeFromCInt,
  )
import GHC.ForeignPtr (mallocPlainForeignPtrAlignedBytes)

data YaboVal
  = YaboNull
  | YaboError YbqReturnStatus
  | YaboBit Bool
  | YaboUnit
  | YaboParser
  | YaboFunction
  | YaboChar Char
  | YaboInt Integer
  | YaboString String
  | YaboArray (Seq YaboVal)
  | YaboBlock (Map String YaboVal)
  deriving (Eq, Ord)


type Globals = ()
withLib :: Library -> (Ptr Globals -> IO a) -> IO a
withLib lib callback = do
  result <- withForeignPtr (globals lib) callback
  touchForeignPtr $ globals lib
  touchForeignPtr $ pointer $ origFile lib
  return result

withReturnBuf :: (Functor f) => Library -> (Ptr Globals -> Ptr () -> IO (f a)) -> IO (f ShortByteString)
withReturnBuf lib f = withLib lib $ \globals -> allocaBytes (maxBuf lib) $ \buf -> do
  functored <- f globals buf
  actualSize <- ybqAllocSize buf
  res <- packCStringLen (castPtr buf, fromIntegral actualSize)
  return $ res <$ functored

arrayIndex :: Library -> ShortByteString -> CSize -> IO YaboVal
arrayIndex lib val idx = do
  let accessElement globals retBuf = do
        _ <- useAsCStringLen val $ \(charptr, _) -> do
          let ptr = castPtr charptr
          ybqArrayAccess retBuf ptr idx globals
        return $ Identity ()

  res <- withReturnBuf lib accessElement
  intoYaboVal lib $ runIdentity res

blockIndex :: Library -> ShortByteString -> CSize -> IO (Maybe (String, YaboVal))
blockIndex lib val idx = useAsCStringLen val $ \(charptr, _) -> runMaybeT $ do
  let ptr = castPtr charptr
  let accessField globals retBuf = do
        status <- ybqFieldAccess retBuf ptr idx globals
        if ybqStatusFromWord64 status == YbqStatusBacktrack
          then return Nothing
          else return $ Just ()

  ret <- MaybeT $ withReturnBuf lib accessField
  fieldName <- liftIO $ ybqFieldNameAtIndex ptr idx >>= fieldNameToString
  retVal <- liftIO $ intoYaboVal lib ret
  return (fieldName, retVal)

intoYaboArray :: Library -> ShortByteString -> Ptr () -> IO YaboVal
intoYaboArray lib val ptr = do
  valsize <- withLib lib $ ybqArraySize ptr
  let access idx = unsafePerformIO $ arrayIndex lib val (fromIntegral idx)
  return $ YaboArray $ fromFunction (fromIntegral valsize) access

intoYaboBlock :: Library -> ShortByteString -> Ptr () -> IO YaboVal
intoYaboBlock lib val ptr = do
  valsize <- ybqFieldCount ptr
  let access idx = unsafePerformIO $ blockIndex lib val (fromIntegral idx)
  let fields = catMaybes $ [access idx | idx <- [0 .. valsize - 1]]
  return $ YaboBlock $ fromList fields

intoYaboVal :: Library -> ShortByteString -> IO YaboVal
intoYaboVal lib rawVal = do
  useAsCStringLen rawVal $ \(charptr, _) -> do
    let ptr = castPtr charptr
    yaboTy <- ybqType ptr
    case ybqTypeFromCInt yaboTy of
      YbqTypeInt -> YaboInt . fromIntegral <$> ybqGetInt ptr
      YbqTypeChar -> YaboChar . toEnum . fromIntegral <$> ybqGetInt ptr
      YbqTypeBit -> YaboBit . (/= 0) <$> ybqGetInt ptr
      YbqTypeArray -> intoYaboArray lib rawVal ptr
      YbqTypeBlock -> intoYaboBlock lib rawVal ptr
      YbqTypeError -> YaboError . ybqStatusFromWord64 <$> ybqGetError ptr
      YbqTypeUnit -> return YaboUnit
      YbqTypeParser -> return YaboParser
      YbqTypeFunction -> return YaboFunction

newtype ParserExport = ParserExport (Ptr ())

data Parser = Parser Library ParserExport

withPackedArgs :: [YaboVal] -> (Ptr () -> IO a) -> IO a
withPackedArgs args action = do
  let int64Args = map yaboValToInt64 args
  let argCount = length int64Args
  let totalBytes = argCount * sizeOf (0 :: Int64)

  allocaBytes totalBytes $ \ptr -> do
    pokeArray (castPtr ptr) int64Args
    action (castPtr ptr)
  where
    yaboValToInt64 :: YaboVal -> Int64
    yaboValToInt64 (YaboInt i) = fromIntegral i
    yaboValToInt64 _ = error "Only integer arguments are supported"

parseWithArgs :: Parser -> Int -> [YaboVal] -> IO YaboVal
parseWithArgs (Parser lib (ParserExport parserExport)) offset args = do
  let offsetFile = atOffset offset (origFile lib)
  asPtrLen offsetFile $ \(charptr, len) -> do
    let filePtr = castPtr charptr

    expectedCount <- ybqExportArgCount parserExport
    if fromIntegral expectedCount /= length args
      then error $ "Expected " ++ show expectedCount ++ " arguments, got " ++ show (length args)
      else do
        withPackedArgs args $ \argsPtr -> do
          ret <- withReturnBuf lib $ \globals retBuf -> do
            _ <- ybqParseBytesWithArgs retBuf filePtr (fromIntegral len) parserExport argsPtr globals
            return $ Identity ()

          intoYaboVal lib $ runIdentity ret

getPtr :: (Monad m) => FunPtr a -> MaybeT m (Ptr b)
getPtr ptr = (MaybeT . return) $ if ptr == nullFunPtr then Nothing else Just (castFunPtrToPtr ptr)

getParser :: Library -> String -> Int -> [YaboVal] -> IO (Maybe YaboVal)
getParser lib name offset args = runMaybeT $ do
  exportPtrPtr <- liftIO $ DL.dlsym (dl lib) name
  exportPtr <- getPtr exportPtrPtr

  let parserExport = ParserExport exportPtr
  liftIO $ parseWithArgs (Parser lib parserExport) offset args

data ByteFile = ByteFile
  { pointer :: ForeignPtr CChar,
    len :: Int
  }

atOffset :: Int -> ByteFile -> ByteFile
atOffset off file = ByteFile (pointer file `plusForeignPtr` off') (len file - off')
  where
    off' = min off $ len file

asPtrLen :: ByteFile -> ((Ptr CChar, Int) -> IO a) -> IO a
asPtrLen (ByteFile ptr len) f = withForeignPtr ptr $ \p -> f (p, len)

data Library = Library
  { dl :: DL.DL,
    origFile :: ByteFile,
    maxBuf :: Int,
    globals :: ForeignPtr Globals
  }



openLibrary :: String -> (ForeignPtr CChar, Int) -> IO (Maybe Library)
openLibrary name file = runMaybeT $ do
  let file' = uncurry ByteFile file
  dl <- liftIO $ DL.dlopen name [DL.RTLD_LAZY, DL.RTLD_LOCAL]
  bufSizeFunPtr <- liftIO $ DL.dlsym dl "yabo_max_buf_size"
  bufSizePtr <- getPtr bufSizeFunPtr
  maxBuf <- liftIO $ peek (bufSizePtr :: Ptr CSize)
  globalsSizeFunPtr <- liftIO $ DL.dlsym dl "yabo_global_size"
  globalsSizePtr <- getPtr globalsSizeFunPtr
  globalsSize <- liftIO $ peek (globalsSizePtr :: Ptr CSize)
  globals <- liftIO $ mallocPlainForeignPtrAlignedBytes (fromIntegral globalsSize) 8
  initFunPtr <- liftIO $ DL.dlsym dl "yabo_global_init"
  initPtr <- getPtr initFunPtr

  status <- liftIO $ withForeignPtr globals $ \globalsPtr -> asPtrLen file' $ \(charptr, len) -> do
    let filePtr = castPtr charptr
    ybqCallInit filePtr (fromIntegral len) initPtr globalsPtr
  liftIO $ touchForeignPtr globals
  liftIO $ touchForeignPtr $ pointer file'

  MaybeT $
    if ybqStatusFromWord64 status /= YbqStatusOk
      then return Nothing
      else return $ Just ()
  return $ Library dl file' (fromIntegral maxBuf) globals

instance Show YaboVal where
  show = toString

toString :: YaboVal -> String
toString (YaboInt a) = show a
toString (YaboChar a) = "\"" ++ [a] ++ "\""
toString (YaboBit True) = "true"
toString (YaboBit False) = "false"
toString (YaboError YbqStatusBacktrack) = "\"backtrack\""
toString (YaboError YbqStatusEos) = "\"eof\""
toString (YaboError _) = "\"error\""
toString (YaboArray a) = "[" ++ intercalate ", " (toString <$> Data.Foldable.toList a) ++ "]"
toString (YaboBlock a) = "{" ++ intercalate ", " ((\(k, v) -> "\"" ++ k ++ "\": " ++ toString v) <$> Data.Map.toList a) ++ "}"
toString (YaboString a) = "\"" ++ a ++ "\""
toString YaboUnit = "\"unit\""
toString YaboParser = "\"parser\""
toString YaboFunction = "\"function\""
toString YaboNull = "null"

{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use lambda-case" #-}
module Main (main) where

import Control.Monad.Trans (liftIO)
import Control.Monad.Trans.Maybe (MaybeT (MaybeT, runMaybeT))
import Data.ByteString qualified as ByteString
import Data.ByteString.Lazy qualified as LazyByteString
import Data.Sequence (Seq (Empty, (:<|)), fromFunction)
import Data.Word (Word8)
import GHC.IO.Handle.FD (stderr)
import Lib (YaboVal (..), getParser, openLibrary, parse)
import System.Environment (getArgs)
import System.IO (hPutStrLn)
import System.IO.MMap (Mode (ReadOnly), mmapFileByteString, mmapFileForeignPtr)
import ValGen (ValGen (Cons, Error))

toChar :: Integer -> Maybe Word8
toChar x = if x > fromChr (maxBound :: Word8) || x < fromChr (minBound :: Word8) then Nothing else Just $ toChr x
  where
    fromChr = toInteger . fromEnum
    toChr = toEnum . fromInteger

asBytes :: Data.Sequence.Seq YaboVal -> LazyByteString.ByteString -> LazyByteString.ByteString
asBytes a acc = case a of
  (YaboInt x) :<| xs -> case toChar x of
    Nothing -> error "Array value not a byte value (out of range)"
    Just xbyte -> LazyByteString.cons xbyte $ asBytes xs acc
  _ :<| _ -> error "Array value not a byte value"
  Empty -> acc

printBin :: ValGen YaboVal -> IO ()
printBin (ValGen.Cons (YaboArray x) xs) = do
  LazyByteString.putStr $ asBytes x LazyByteString.empty
  printBin xs
printBin (ValGen.Cons _ _) = do
  hPutStrLn stderr "Could not print, not an array of bytes"
printBin (ValGen.Error res) = putStr ("Error: " ++ res)
printBin _ = pure ()

procLine :: Bool -> YaboVal -> String -> IO ()
procLine binary parser line = do
  parseResult <- parse line "stdin"
  case parseResult of
    Left err -> print err
    Right program -> if not binary then print $ program parser else printBin $ program parser

withMaybeIO :: MaybeT IO () -> IO ()
withMaybeIO prog = runMaybeT prog >>= maybe (pure ()) return

main :: IO ()
main = withMaybeIO $ do
  args <- liftIO getArgs
  (binary, parser) <- case args of
    [library, parser, input] -> (False,) <$> loadParser library parser input
    ["-b", library, parser, input] -> (True,) <$> loadParser library parser input
    [input] -> (False,) <$> loadFlat input
    ["-b", input] -> (True,) <$> loadFlat input
    _ -> do
      liftIO $ putStrLn "Usage: ybq [-b] [library parser] <input>"
      MaybeT $ return Nothing
  inp <- liftIO getContents
  liftIO $ mapM_ (procLine binary parser) $ filter (/= "") (lines inp)
  where
    loadParser libName parserName inputPath = do
      (file, _, size) <- liftIO $ mmapFileForeignPtr inputPath ReadOnly Nothing
      lib <- MaybeT $ openLibrary libName (file, size)
      MaybeT $ getParser lib parserName 0
    loadFlat inputPath = do
      file <- liftIO $ mmapFileByteString inputPath Nothing
      let arr = fromFunction (ByteString.length file) $ YaboInt . toInteger . ByteString.index file
      return $ YaboArray arr

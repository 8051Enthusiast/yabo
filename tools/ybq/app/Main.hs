{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use lambda-case" #-}
module Main (main) where

import Control.Monad.Trans (liftIO)
import Control.Monad.Trans.Maybe (MaybeT (MaybeT, runMaybeT))
import Data.ByteString (ByteString, pack)
import Data.ByteString qualified as ByteString
import Data.Foldable (toList)
import Data.Sequence (fromFunction)
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

valToBinary :: YaboVal -> Maybe ByteString
valToBinary (YaboArray s) =
  pack . toList
    <$> mapM
      ( \x ->
          case x of
            YaboInt i -> toChar i
            _ -> Nothing
      )
      s
valToBinary _ = Nothing

printBin :: ValGen YaboVal -> IO ()
printBin (ValGen.Cons x xs) = case valToBinary x of
  Just bin -> do
    ByteString.putStr bin
    printBin xs
  Nothing -> do
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

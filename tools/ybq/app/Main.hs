{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use lambda-case" #-}
module Main (main) where

import Control.Monad.Trans (liftIO)
import Control.Monad.Trans.Maybe (MaybeT (MaybeT, runMaybeT))
import Data.ByteString (ByteString, pack)
import Data.ByteString qualified as ByteString
import Data.Foldable (toList)
import Data.Word (Word8)
import GHC.IO.Handle.FD (stderr)
import Lib (YaboVal (..), getParser, openLibrary, parse)
import System.Environment (getArgs)
import System.IO (hPutStrLn)
import System.IO.MMap (Mode (ReadOnly), mmapFileForeignPtr)
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

main :: IO ()
main =
  runMaybeT prog >>= maybe (pure ()) return
  where
    prog = do
      args <- liftIO getArgs
      (binary, libName, parserName, inputFile) <- case args of
        [library, parser, input] -> return (False, library, parser, input)
        ["-b", library, parser, input] -> return (True, library, parser, input)
        _ -> do
          liftIO $ putStrLn "Usage: ybq [-b] <library> <parser> <input>"
          MaybeT $ return Nothing
      (file, _, size) <- liftIO $ mmapFileForeignPtr inputFile ReadOnly Nothing
      lib <- MaybeT $ openLibrary libName (file, size)
      parser <- MaybeT $ getParser lib parserName 0
      inp <- liftIO getContents
      liftIO $ mapM_ (procLine binary parser) $ filter (/= "") (lines inp)

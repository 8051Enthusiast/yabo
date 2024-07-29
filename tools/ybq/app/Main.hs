module Main (main) where

import Control.Monad.Trans (liftIO)
import Control.Monad.Trans.Maybe (MaybeT (MaybeT, runMaybeT))
import Lib (YaboVal, getParser, openLibrary, parse)
import System.Environment (getArgs)
import System.IO.MMap (mmapFileByteString)

procLine :: (Int -> YaboVal) -> String -> IO ()
procLine parser line = do
  parseResult <- parse line "stdin"
  case parseResult of
    Left err -> print err
    Right program -> print $ program $ parser 0

main :: IO ()
main =
  runMaybeT prog >>= maybe (pure ()) return
  where
    prog = do
      args <- liftIO getArgs
      (libName, parserName, inputFile) <- case args of
        [library, parser, input] -> return (library, parser, input)
        _ -> do
          liftIO $ putStrLn "Usage: ybq <library> <parser> <input>"
          MaybeT $ return Nothing
      file <- liftIO $ mmapFileByteString inputFile Nothing
      lib <- MaybeT $ openLibrary libName file
      parser <- MaybeT $ getParser lib parserName
      inp <- liftIO getContents
      liftIO $ mapM_ (procLine parser) (lines inp)

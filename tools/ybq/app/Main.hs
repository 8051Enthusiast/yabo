module Main (main) where

import Control.Monad.Trans (liftIO)
import Control.Monad.Trans.Maybe (MaybeT (MaybeT, runMaybeT))
import Lib (getParser, openLibrary, parse)
import System.Environment (getArgs)
import System.IO.MMap (mmapFileByteString)

main :: IO ()
main =
  runMaybeT
    ( do
        args <- liftIO getArgs
        let (libName, parserName, program, inputFile) = case args of
              [library, parser, prog, input] -> (library, parser, prog, input)
              _ -> error "Usage: ybq <library> <parser> <program> <input>"
        parseResult <- liftIO $ parse program "cmdline"
        parsed <- case parseResult of
          Left err -> do
            liftIO $ print err
            MaybeT $ return Nothing
          Right p -> return p
        file <- liftIO $ mmapFileByteString inputFile Nothing
        lib <- MaybeT $ openLibrary libName file
        parser <- MaybeT $ getParser lib parserName
        let res = parser 0
        liftIO $ print $ parsed res
    )
    >>= maybe (pure ()) return

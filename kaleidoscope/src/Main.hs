{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Monad.Trans ( MonadIO(liftIO) )
-- import Emit

import Data.Text (pack, strip, unpack)
import ParserH ( parseToplevel )
import StdLibrary ( generateLibraries )
import Options.Applicative
import CLI (getNextInput, opts, CliOptions (inputFile))
import System.Console.Haskeline
import Processor ( process )
import qualified LLVM.AST as AST
import LLVM.AST (Definition)

processFile :: String -> CliOptions -> [Definition] -> IO (Maybe [AST.Definition])
processFile fname processOptions defs = do
  file <- readFile fname
  result <- process defs file processOptions
  return $ snd <$> result

repl :: CliOptions -> [Definition] -> IO ()
repl replOptions initialDefs = runInputT defaultSettings (loop "0" initialDefs)
  where
    loop prevRes oldDefs = do
      outputStr "ready> "
      minput <- getNextInput
      case minput of
        Nothing -> outputStrLn "Goodbye."
        Just input -> do
          case unpack $ strip $ pack input of
            ('=' : rest) -> do
              maybeDefs <- liftIO $ process oldDefs ("const " ++ removeLast rest ++ " " ++ show prevRes ++ ";") replOptions
              case maybeDefs of
                Just (_, defs) -> loop prevRes defs
                Nothing -> loop prevRes oldDefs
            _ -> do
              maybeDefs <- liftIO $ process oldDefs input replOptions
              case maybeDefs of
                Just (res, defs) -> loop res defs
                Nothing -> loop prevRes oldDefs
    removeLast :: String -> String
    removeLast [] = []
    removeLast [_] = []
    removeLast (x : xs) = x : removeLast xs

main :: IO ()
main = do
  parsedOptions <- execParser opts
  defs <- generateLibraries
  case defs of
    Just definitions -> do
      case inputFile parsedOptions of
        [] -> repl parsedOptions definitions
        fname -> processFile fname parsedOptions definitions >> return ()
    Nothing -> error "Could not process library"

-- Print AST (chapter 2)
printAST :: String -> IO ()
printAST line = do
  let res = parseToplevel line
  case res of
    Left err -> print err
    Right ex -> mapM_ print ex

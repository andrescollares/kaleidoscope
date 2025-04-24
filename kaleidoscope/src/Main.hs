-- {-# LANGUAGE OverloadedStrings #-}

module Main where

import CLIParameters (CLIParameters (..), parserParameters)
import Control.Monad (void)
import Control.Monad.Trans (MonadIO (liftIO))
import Data.Text (pack, strip, unpack)
import qualified LLVM.AST as AST (Definition)
import Options.Applicative
import Processor (process)
import StdLib.GenLibs (generateLibraries)
import System.Console.Haskeline (InputT, defaultSettings, getInputLine, outputStr, outputStrLn, runInputT)

main :: IO ()
main = do
  cliParameters <- execParser parserInfo
  case inputFile cliParameters of
    [] -> startRepl cliParameters
    fname -> void (processFile fname cliParameters)

parserInfo :: ParserInfo CLIParameters
parserInfo =
  info
    (parserParameters <**> helper)
    (fullDesc <> progDesc "Kaleidoscope compiler" <> header "Kaleidoscope compiler")

startRepl :: CLIParameters -> IO ()
startRepl cliParameters = do
  libs <- generateLibraries
  runInputT defaultSettings (loop libs)
  where
    loop oldDefs = do
      outputStr "ready> "
      minput <- getNextInput
      case minput of
        Nothing -> outputStrLn "Goodbye."
        Just input -> do
          case unpack $ strip $ pack input of
            (':' : 'l' : ' ' : fileName) -> do
              maybeDefs <- liftIO $ processFile cleanFileName cliParameters
              case maybeDefs of
                Just defs -> loop defs
                Nothing -> loop oldDefs
              where
                -- because ";" must be present in the end of the line
                cleanFileName = removeLast fileName
                removeLast :: String -> String
                removeLast [] = []
                removeLast [_] = []
                removeLast (x : xs) = x : removeLast xs
            _ -> do
              maybeDefs <- liftIO $ process oldDefs input cliParameters
              case maybeDefs of
                Just defs -> do
                  loop defs
                Nothing -> loop oldDefs

getNextInput :: InputT IO (Maybe String)
getNextInput = do
  nextInputLine <- getInputLine ""
  case nextInputLine of
    Just line -> case lastCharOrEmpty line of
      ';' -> return $ Just line
      _ -> do
        nextLine <- getNextInput
        case nextLine of
          Nothing -> return $ Just line
          Just next -> return $ Just $ line ++ ' ' : next
    Nothing -> return Nothing
  where
    lastCharOrEmpty :: String -> Char
    lastCharOrEmpty [] = ' '
    lastCharOrEmpty s = last s

processFile :: String -> CLIParameters -> IO (Maybe [AST.Definition])
processFile fname cliParameters = do
  file <- readFile fname
  libs <- generateLibraries
  process libs file cliParameters
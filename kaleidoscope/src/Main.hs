-- {-# LANGUAGE OverloadedStrings #-}

module Main where

import CLIParameters (CLIParameters (..))
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

parserParameters :: Parser CLIParameters
parserParameters =
  CLIParameters
    <$> option
      auto
      ( long "opt-level"
          <> short 'o'
          <> metavar "OPT"
          <> value 3
          <> help "Optimization level 0-3"
      )
    <*> strOption
      ( long "file"
          <> short 'f'
          <> metavar "FILE"
          <> value []
          <> help "File to read from"
      )
    <*> flag
      True
      False
      ( long "quiet-llvm"
          <> short 'q'
          <> help "Hide LLVM IR output"
      )
    <*> flag
      False
      True
      ( long "fail-on-errors"
          <> short 'e'
          <> help "Fail on errors"
      )

startRepl :: CLIParameters -> IO ()
startRepl cliParameters = do
  libs <- generateLibraries
  runInputT defaultSettings (loop "0" libs)
  where
    loop prevRes oldDefs = do
      outputStr "ready> "
      minput <- getNextInput
      case minput of
        Nothing -> outputStrLn "Goodbye."
        Just input -> do
          case unpack $ strip $ pack input of
            -- TODO: remove this awful idea
            ('=' : rest) -> do
              maybeDefs <- liftIO $ process oldDefs ("const " ++ removeLast rest ++ " " ++ show prevRes ++ ";") cliParameters
              case maybeDefs of
                Just (_, defs) -> loop prevRes defs
                Nothing -> loop prevRes oldDefs
            -- TODO: load file definitions
            -- ':' : 'l' : ' ' : fileName -> do
            --   return $ processFile fileName cliParameters
            _ -> do
              maybeDefs <- liftIO $ process oldDefs input cliParameters
              case maybeDefs of
                Just (res, defs) -> loop res defs
                Nothing -> loop prevRes oldDefs
    removeLast :: String -> String
    removeLast [] = []
    removeLast [_] = []
    removeLast (x : xs) = x : removeLast xs

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

lastCharOrEmpty :: String -> Char
lastCharOrEmpty [] = ' '
lastCharOrEmpty s = last s

-- TODO: Compile to source file (.o or smth)
processFile :: String -> CLIParameters -> IO (Maybe [AST.Definition])
processFile fname cliParameters = do
  file <- readFile fname
  libs <- generateLibraries
  result <- process libs file cliParameters
  return $ snd <$> result
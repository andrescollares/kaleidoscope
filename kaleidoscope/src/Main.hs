-- {-# LANGUAGE OverloadedStrings #-}

module Main where

import CLIParameters (CLIParameters (..))
import Control.Monad (void)
import Options.Applicative
import Repl (repl)
import Srep (processFile)
import StdLib.GenLibs (generateLibraries)

main :: IO ()
main = do
  cliParameters <- execParser parserInfo
  defs <- generateLibraries
  case defs of
    Just definitions -> do
      case inputFile cliParameters of
        [] -> repl cliParameters definitions
        fname -> void (processFile fname cliParameters definitions)
    Nothing -> error "Could not process library"

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
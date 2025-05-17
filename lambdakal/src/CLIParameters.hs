module CLIParameters where

import Options.Applicative

data CLIParameters = CLIParameters
  { optimizationLevel :: Word,
    inputFile :: String,
    emitLLVM :: Bool,
    emitAST :: Bool,
    emitLlvmDefs :: Bool,
    failOnErrors :: Bool,
    compile :: Bool
  }

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
      False
      True
      ( long "llvm"
          <> short 'l'
          <> help "Show LLVM IR output"
      )
    <*> flag
      False
      True
      ( long "ast"
          <> short 'a'
          <> help "Show AST representation"
      )
    <*> flag
      False
      True
      ( long "defs"
          <> short 'd'
          <> help "Show LLVM Module definitions"
      )
    <*> flag
      False
      True
      ( long "fail-on-errors"
          <> short 'e'
          <> help "Fail on errors"
      )
    <*> flag
      False
      True
      ( long "compile"
          <> short 'c'
          <> help "Compile to native code"
      )

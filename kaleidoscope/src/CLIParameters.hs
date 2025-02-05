module CLIParameters where
import Options.Applicative

data CLIParameters = CLIParameters
  { optimizationLevel :: Word,
    inputFile :: String,
    emitLLVM :: Bool,
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
    <*> flag
      False
      True
      ( long "compile"
          <> short 'c'
          <> help "Compile to native code"
      )
  
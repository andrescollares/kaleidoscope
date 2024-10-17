module CLIParameters where

data CLIParameters = CLIParameters
  { optimizationLevel :: Word,
    inputFile :: String,
    emitLLVM :: Bool,
    failOnErrors :: Bool
  }
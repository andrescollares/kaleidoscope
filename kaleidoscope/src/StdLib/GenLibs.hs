{-# OPTIONS_GHC -Wno-incomplete-record-updates #-}

module StdLib.GenLibs where

import CLIParameters (CLIParameters (..))
import LLVM.AST (Definition)
import Processor (process)
import StdLib.BaseDefs (baseDefinitions)

generateLibraries :: IO (Maybe [Definition])
generateLibraries = do
  defs <- mapM processLibrary files
  return $ concat <$> sequence defs

processLibrary :: String -> IO (Maybe [Definition])
processLibrary fname = do
  file <- readFile fname
  result <- process baseDefinitions file (CLIParameters {inputFile = "", failOnErrors = False, optimizationLevel = 3, emitLLVM = False})
  return $ snd <$> result

files :: [String]
files = ["./src/StdLib/array.k"]
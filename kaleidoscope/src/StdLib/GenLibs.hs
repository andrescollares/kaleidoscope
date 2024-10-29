{-# OPTIONS_GHC -Wno-incomplete-record-updates #-}

module StdLib.GenLibs where

import CLIParameters (CLIParameters (..))
import LLVM.AST (Definition)
import Processor (process)
import StdLib.BaseDefs (baseDefinitions)

generateLibraries :: IO [Definition]
generateLibraries = fmap concat (mapM processLibrary libFiles)

processLibrary :: String -> IO [Definition]
processLibrary fname = do
  file <- readFile fname
  result <- process baseDefinitions file (CLIParameters {inputFile = "", failOnErrors = False, optimizationLevel = 3, emitLLVM = False})
  case result of
    Just (_, definitions) -> return definitions
    Nothing -> error "Could not process library"

libFiles :: [String]
libFiles = ["./src/StdLib/list.k"]
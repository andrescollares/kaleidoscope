{-# OPTIONS_GHC -Wno-incomplete-record-updates #-}

module StdLib.GenLibs where

import CLIParameters (CLIParameters (..))
import LLVM.AST (Definition)
import Processor (process)
import StdLib.BaseDefs (baseDefinitions)

generateLibraries :: IO [Definition]
generateLibraries = do
  source <- generateSource
  processLibrary source

generateSource :: IO String
generateSource = fmap concat (mapM readFile libFiles)

processLibrary :: String -> IO [Definition]
processLibrary source = do
  result <- process baseDefinitions source (CLIParameters {inputFile = "", failOnErrors = False, optimizationLevel = 3, emitLLVM = False})
  case result of
    Just definitions -> return definitions
    Nothing -> error "Could not process library"

libFiles :: [String]
libFiles = ["./src/StdLib/list.k", "./src/StdLib/tuple.k"]
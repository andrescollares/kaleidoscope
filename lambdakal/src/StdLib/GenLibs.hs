module StdLib.GenLibs where

import CLIParameters (CLIParameters (..))
import Data.List (isSuffixOf)
import LLVM.AST (Definition)
import Processor (process)
import StdLib.BaseDefs (baseDefinitions)
import System.Directory (getDirectoryContents)

generateLibraries :: IO [Definition]
generateLibraries = do
  source <- generateSource
  processLibrary source

generateSource :: IO String
generateSource = do
  files <- libFiles
  sources <- mapM readFile files
  return $ unlines sources

processLibrary :: String -> IO [Definition]
processLibrary source = do
  result <- process baseDefinitions source (CLIParameters {inputFile = "", failOnErrors = False, optimizationLevel = 3, emitLLVM = False, emitAST = False, emitLlvmDefs = False, compile = False})
  case result of
    Just definitions -> return definitions
    Nothing -> error "Could not process library"

libFiles :: IO [String]
libFiles = do
  let libDir = "./src/StdLib/lib"
  files <- getDirectoryContents libDir
  return $ map ((libDir ++ "/") ++) (filter (\s -> ".k" `isSuffixOf` s) files)
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Monad (void)
import Data.List (isSuffixOf)
import LLVM.IRBuilder.Module (buildModule)
import System.Directory (doesFileExist, getDirectoryContents, listDirectory)
import System.Exit (ExitCode (ExitSuccess))
import System.FilePath ((</>))
import System.Process (system)
import Test.Tasty (TestTree, defaultMain, localOption, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))
import Test.Tasty.Runners (NumThreads (NumThreads))

-- Main test suite
main :: IO ()
main = do
    files <- listDirectory programDir
    let testFiles = filterProgramFiles files
    putStrLn $ "Found " ++ show (length testFiles) ++ " test files."
    
    let testCases = map generateOptimizationVariants testFiles
    defaultMain $ localOption (NumThreads 1) $ testGroup "Program Tests" testCases

-- Directory paths
programDir :: FilePath
programDir = "./test/programs"

outputDir :: FilePath
outputDir = "./test/output"

tempOutput :: FilePath
tempOutput = "./test/tmp.out"

silentCabal :: String
silentCabal = "cabal run -v0 kaleidoscope-fing -- --file="

-- tests :: TestTree
-- tests = testGroup "Tests" [programTests]

filterProgramFiles :: [String] -> [String]
filterProgramFiles = filter (\s -> ".k" `isSuffixOf` s)

generateOptimizationVariants :: FilePath -> TestTree
generateOptimizationVariants file = testGroup file $ map (generateTest file) ["-o0", "-o3"]

-- Generate individual tests for each program file
generateTest :: FilePath -> String -> TestTree
generateTest file optLevel = testCase ("\t " ++ file ++ " " ++ optLevel) $ do
    let filePath = programDir </> file
        outputFilePath = outputDir </> file
    
    -- Run the test command
    exitCode <- system $ silentCabal ++ filePath ++ " " ++ optLevel ++ " --fail-on-errors > " ++ tempOutput
    exitCode @?= ExitSuccess

    -- Read actual output
    actualOutput <- readFile tempOutput

    -- Check if expected output file exists
    fileExists <- doesFileExist outputFilePath
    if fileExists
      then do
        expectedOutput <- readFile outputFilePath
        actualOutput @?= expectedOutput
      else do
        putStrLn $ "No expected output file for " ++ file
        putStrLn $ "Writing actual output to " ++ outputFilePath
        writeFile outputFilePath actualOutput

    -- Final comparison to ensure correctness
    expectedOutput <- readFile outputFilePath
    actualOutput @?= expectedOutput

    -- Cleanup
    removeFile tempOutput

removeFile :: FilePath -> IO ()
removeFile file = do
  fileExists <- doesFileExist file
  if fileExists
    then do
      system $ "rm " ++ file
      return ()
    else return ()

-- TODO: REPL features tests

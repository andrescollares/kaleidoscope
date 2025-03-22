{-# LANGUAGE OverloadedStrings #-}

module Main where

import Test.Tasty ( defaultMain, testGroup, TestTree, localOption )
import Test.Tasty.HUnit ( testCase, (@?=) )

import LLVM.IRBuilder.Module (buildModule)
import Control.Monad (void)
import System.Process ( system )
import System.Exit ( ExitCode(ExitSuccess) )
import System.Directory ( doesFileExist, getDirectoryContents, listDirectory )
import Data.List (isSuffixOf)
import System.FilePath ((</>))
import Test.Tasty.Runners (NumThreads(NumThreads))

-- Main test suite
main :: IO ()
main = do
    files <- listDirectory programDir
    let testFiles = filterProgramFiles files
    putStrLn $ "Found " ++ show (length testFiles) ++ " test files."
    
    let testCases = map generateTest testFiles
    defaultMain $ localOption (NumThreads 1) $ testGroup "Program Tests" testCases

-- Directory paths
programDir :: FilePath
programDir = "./test/programs"

outputDir :: FilePath
outputDir = "./test/output"

tempOutput :: FilePath
tempOutput = "./test/tmp.out"

-- tests :: TestTree
-- tests = testGroup "Tests" [programTests]

filterProgramFiles :: [String] -> [String]
filterProgramFiles = filter (\s -> ".k" `isSuffixOf` s)

-- Generate individual tests for each program file
generateTest :: FilePath -> TestTree
generateTest file = testCase ("Testing " ++ file) $ do
    putStrLn $ "Testing file: " ++ file
    let filePath = programDir </> file
        outputFilePath = outputDir </> file
    
    -- Run the test command
    exitCode <- system $ "cabal run kaleidoscope-fing -- --file=" ++ filePath ++ " --fail-on-errors > " ++ tempOutput
    putStrLn $ "\tExit code: " ++ show exitCode
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
        putStrLn $ "Removing file: " ++ file
        system $ "rm " ++ file
        return ()
      else return ()
  
-- TODO: REPL features tests

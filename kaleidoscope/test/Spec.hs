{-# LANGUAGE OverloadedStrings #-}

module Main where

import Test.Tasty ( defaultMain, testGroup, TestTree )
import Test.Tasty.HUnit ( testCase, (@?=) )

import LLVM.IRBuilder.Module (buildModule)
import Control.Monad (void)
import System.Process ( system )
import System.Exit ( ExitCode(ExitSuccess) )
import System.Directory ( doesFileExist, getDirectoryContents )
import Data.List (isSuffixOf)

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests" [programTests]

filterProgramFiles :: [String] -> [String]
filterProgramFiles = filter (\s -> ".k" `isSuffixOf` s)

programTests :: TestTree
programTests = testCase "Program Tests" (do
  files <- getDirectoryContents "./test/programs"
  putStrLn $ "Testing " ++ show (length $ filterProgramFiles files) ++ " files"
  system $ "echo Testing files: " ++ show (filterProgramFiles files)
  mapM_ (\f -> do
    system $ "echo Testing file: " ++ f
    exitCode <- system $ "cabal run kaleidoscope-fing -- --file=./test/programs/" ++ f ++ " --quiet-llvm --fail-on-errors > ./test/tmp.out"
    putStrLn $ "\tExit code: " ++ show exitCode
    exitCode @?= ExitSuccess
    actualOutput <- readFile "./test/tmp.out"
    fileExists <- doesFileExist ("./test/output/" ++ f)
    if fileExists
      then do
        expectedOutput <- readFile $ "./test/output/" ++ f
        actualOutput @?= expectedOutput
      else do
        putStrLn $ "No expected output file for " ++ f
        putStrLn $ "Writing actual output to ./test/output/" ++ f
        writeFile ("./test/output/" ++ f) actualOutput
    expectedOutput <- readFile $ "./test/output/" ++ f
    actualOutput @?= expectedOutput
    system "rm ./test/tmp.out"
    ) (filterProgramFiles files)
  )

-- TODO: REPL features tests

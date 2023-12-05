module Main where

import Codegen
import Control.Monad.Trans
import Data.String
-- import Emit

import IRBuilder (genModule)
import qualified LLVM.AST as AST
import ParserH
import System.Console.Haskeline
import System.Environment

initModule :: AST.Module
initModule = emptyModule $ fromString "Kaleidoscope"

process :: [AST.Definition] -> String -> IO (Maybe [AST.Definition])
process oldDefs source = do
  let res = parseToplevel source
  case res of
    Left err -> print err >> return Nothing
    Right expressions -> do
      defs <- genModule oldDefs expressions
      return $ Just defs 

processFile :: String -> IO (Maybe [AST.Definition])
processFile fname = readFile fname >>= process []

repl :: IO ()
repl = runInputT defaultSettings (loop [])
  where
    loop oldDefs = do
      minput <- getInputLine "ready> "
      case minput of
        Nothing -> outputStrLn "Goodbye."
        Just input -> do
          maybeDefs <- liftIO $ process oldDefs input
          case maybeDefs of
            Just defs -> loop defs
            Nothing -> loop oldDefs

main :: IO ()
main = do
  args <- getArgs
  case args of
    [] -> repl
    [fname] -> processFile fname >> return ()
    _ -> print "Usage: kaleidoscope [filename]"

-- Print AST (chapter 2)
printAST :: String -> IO ()
printAST line = do
  let res = parseToplevel line
  case res of
    Left err -> print err
    Right ex -> mapM_ print ex

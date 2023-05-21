module Main where

import Codegen
import Control.Monad.Trans
import Data.String
import Emit
import qualified LLVM.AST as AST
import ParserH
import System.Console.Haskeline
import System.Environment


initModule :: AST.Module
initModule = emptyModule $ fromString "Kaleidoscope"

process :: AST.Module -> String -> IO (Maybe AST.Module)
process modo source = do
  let res = parseToplevel source
  case res of
    Left err -> print err >> return Nothing
    Right expressions -> do
      ast <- codegen modo expressions
      return $ Just ast

processFile :: String -> IO (Maybe AST.Module)
processFile fname = readFile fname >>= process initModule

repl :: IO ()
repl = runInputT defaultSettings (loop initModule)
  where
    loop modl = do
      minput <- getInputLine "ready> "
      case minput of
        Nothing -> outputStrLn "Goodbye."
        Just input -> do
          maybeModlName <- liftIO $ process modl input
          case maybeModlName of
            Just modlName -> loop modlName
            Nothing -> loop modl

main :: IO ()
main = do
  args <- getArgs
  case args of
    [] -> repl
    [fname] -> processFile fname >> return ()
    _ -> print "Usage: kaleidoscope [filename]"

-- Imprimir el AST (chapter 2)
printAST :: String -> IO ()
printAST line = do
  let res = parseToplevel line
  case res of
    Left err -> print err
    Right ex -> mapM_ print ex

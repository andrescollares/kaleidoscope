module Main where

import Control.Monad.Trans
-- import Emit

import IRBuilder.GenModule (genModule)
import qualified LLVM.AST as AST
import ParserH
import System.Console.Haskeline
import System.Environment
import Data.Text (strip, unpack, pack)

import StdLibrary


process :: [AST.Definition] -> String -> IO (Maybe (Double, [AST.Definition]))
process oldDefs source = do
  let parsedSrc = parseToplevel source
  case parsedSrc of
    Left err -> print err >> return Nothing
    Right expressions -> do
      (res, defs) <- genModule oldDefs expressions
      return $ Just (res, defs)

processFile :: String -> IO (Maybe [AST.Definition])
processFile fname = do
  file <- readFile fname
  result <- process [] file
  return $ snd <$> result

repl :: IO ()
repl = runInputT defaultSettings (loop 0 stdLibrary)
  where
    loop prevRes oldDefs = do
      minput <- getInputLine "ready> "
      case minput of
        Nothing -> outputStrLn "Goodbye."
        Just input -> do
          case unpack $ strip $ pack input of
            ('=':rest) -> do
              maybeDefs <- liftIO $ process oldDefs ("const " ++ removeLast rest ++ " " ++ show prevRes ++ ";")
              case maybeDefs of
                Just (_, defs) -> loop prevRes defs
                Nothing -> loop prevRes oldDefs
            _ -> do
              maybeDefs <- liftIO $ process oldDefs input
              case maybeDefs of
                Just (res, defs) -> loop res defs
                Nothing -> loop prevRes oldDefs
    removeLast :: String -> String
    removeLast [] = []
    removeLast [_] = []
    removeLast (x:xs) = x : removeLast xs

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

module Main where

import Control.Monad.Trans ( MonadIO(liftIO) )
-- import Emit

import Data.Text (pack, strip, unpack)
import IRBuilder.GenModule (genModule)
import qualified LLVM.AST as AST
import ParserH ( parseToplevel )
import StdLibrary ( stdLibrary )
import System.Console.Haskeline
    ( defaultSettings, getInputLine, outputStrLn, runInputT )
import System.Environment ( getArgs )

process :: [AST.Definition] -> String -> Word -> IO (Maybe (String, [AST.Definition]))
process oldDefs source optLevel = do
  let parsedSrc = parseToplevel source
  case parsedSrc of
    Left err -> print err >> return Nothing
    Right expressions -> do
      (res, defs) <- genModule oldDefs expressions optLevel
      return $ Just (res, defs)

processFile :: String -> Word -> IO (Maybe [AST.Definition])
processFile fname optLevel = do
  file <- readFile fname
  result <- process [] file optLevel
  return $ snd <$> result

replOptLevel :: Word
replOptLevel = 3

repl :: IO ()
repl = runInputT defaultSettings (loop "0" stdLibrary)
  where
    loop prevRes oldDefs = do
      minput <- getInputLine "ready> "
      case minput of
        Nothing -> outputStrLn "Goodbye."
        Just input -> do
          case unpack $ strip $ pack input of
            ('=' : rest) -> do
              maybeDefs <- liftIO $ process oldDefs ("const " ++ removeLast rest ++ " " ++ show prevRes ++ ";") replOptLevel
              case maybeDefs of
                Just (_, defs) -> loop prevRes defs
                Nothing -> loop prevRes oldDefs
            _ -> do
              maybeDefs <- liftIO $ process oldDefs input replOptLevel
              case maybeDefs of
                Just (res, defs) -> loop res defs
                Nothing -> loop prevRes oldDefs
    removeLast :: String -> String
    removeLast [] = []
    removeLast [_] = []
    removeLast (x : xs) = x : removeLast xs

main :: IO ()
main = do
  args <- getArgs
  case args of
    [] -> repl
    [fname] -> processFile fname 3 >> return ()
    [fname, optLevel] -> processFile fname (read optLevel) >> return ()
    _ -> print "Usage: kaleidoscope [filename]"

-- Print AST (chapter 2)
printAST :: String -> IO ()
printAST line = do
  let res = parseToplevel line
  case res of
    Left err -> print err
    Right ex -> mapM_ print ex

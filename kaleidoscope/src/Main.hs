{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Monad.Trans ( MonadIO(liftIO) )
-- import Emit

import Data.Text (pack, strip, unpack)
import IRBuilder.GenModule (genModule)
import qualified LLVM.AST as AST
import ParserH ( parseToplevel )
import StdLibrary ( stdLibrary )
import Options.Applicative
import CLI (getNextInput, opts, CliOptions (inputFile))
import System.Console.Haskeline

process :: [AST.Definition] -> String -> CliOptions -> IO (Maybe (String, [AST.Definition]))
process oldDefs source processOptions = do
  let parsedSrc = parseToplevel source 
  case parsedSrc of
    Left err -> print err >> return Nothing
    Right expressions -> do
      (res, defs) <- genModule oldDefs expressions processOptions
      return $ Just (res, defs)

processFile :: String -> CliOptions -> IO (Maybe [AST.Definition])
processFile fname processOptions = do
  file <- readFile fname
  result <- process [] file processOptions
  return $ snd <$> result

repl :: CliOptions -> IO ()
repl replOptions = runInputT defaultSettings (loop "0" stdLibrary)
  where
    loop prevRes oldDefs = do
      outputStr "ready> "
      minput <- getNextInput
      case minput of
        Nothing -> outputStrLn "Goodbye."
        Just input -> do
          case unpack $ strip $ pack input of
            ('=' : rest) -> do
              maybeDefs <- liftIO $ process oldDefs ("const " ++ removeLast rest ++ " " ++ show prevRes ++ ";") replOptions
              case maybeDefs of
                Just (_, defs) -> loop prevRes defs
                Nothing -> loop prevRes oldDefs
            _ -> do
              maybeDefs <- liftIO $ process oldDefs input replOptions
              case maybeDefs of
                Just (res, defs) -> loop res defs
                Nothing -> loop prevRes oldDefs
    removeLast :: String -> String
    removeLast [] = []
    removeLast [_] = []
    removeLast (x : xs) = x : removeLast xs

main :: IO ()
main = do
  parsedOptions <- execParser opts
  case inputFile parsedOptions of
    [] -> repl parsedOptions
    fname -> processFile fname parsedOptions >> return ()

-- Print AST (chapter 2)
printAST :: String -> IO ()
printAST line = do
  let res = parseToplevel line
  case res of
    Left err -> print err
    Right ex -> mapM_ print ex

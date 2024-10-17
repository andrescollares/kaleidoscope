module Repl where

import CLIParameters (CLIParameters)
import Control.Monad.Trans (MonadIO (liftIO))
import Data.Text (pack, strip, unpack)
import LLVM.AST (Definition)
import Processor (process)
import System.Console.Haskeline

repl :: CLIParameters -> [Definition] -> IO ()
repl cliParameters initialDefs = runInputT defaultSettings (loop "0" initialDefs)
  where
    loop prevRes oldDefs = do
      outputStr "ready> "
      minput <- getNextInput
      case minput of
        Nothing -> outputStrLn "Goodbye."
        Just input -> do
          case unpack $ strip $ pack input of
            ('=' : rest) -> do
              maybeDefs <- liftIO $ process oldDefs ("const " ++ removeLast rest ++ " " ++ show prevRes ++ ";") cliParameters
              case maybeDefs of
                Just (_, defs) -> loop prevRes defs
                Nothing -> loop prevRes oldDefs
            _ -> do
              maybeDefs <- liftIO $ process oldDefs input cliParameters
              case maybeDefs of
                Just (res, defs) -> loop res defs
                Nothing -> loop prevRes oldDefs
    removeLast :: String -> String
    removeLast [] = []
    removeLast [_] = []
    removeLast (x : xs) = x : removeLast xs

getNextInput :: InputT IO (Maybe String)
getNextInput = do
  nextInputLine <- getInputLine ""
  case nextInputLine of
    Just line -> case lastCharOrEmpty line of
      ';' -> return $ Just line
      _ -> do
        nextLine <- getNextInput
        case nextLine of
          Nothing -> return $ Just line
          Just next -> return $ Just $ line ++ ' ' : next
    Nothing -> return Nothing

lastCharOrEmpty :: String -> Char
lastCharOrEmpty [] = ' '
lastCharOrEmpty s = last s
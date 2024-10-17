module Srep where
-- Single read eval print

import LLVM.AST as AST (Definition)
import CLIParameters (CLIParameters (..))
import Processor (process)

processFile :: String -> CLIParameters -> [AST.Definition] -> IO (Maybe [AST.Definition])
processFile fname cliParameters defs = do
  file <- readFile fname
  result <- process defs file cliParameters
  return $ snd <$> result
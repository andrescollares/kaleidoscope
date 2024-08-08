module Processor where

import qualified LLVM.AST as AST
import CLI (CliOptions (..))
import ParserH (parseToplevel)
import IRBuilder.GenModule (genModule)

process :: [AST.Definition] -> String -> CliOptions -> IO (Maybe (String, [AST.Definition]))
process oldDefs source processOptions = do
  let parsedSrc = parseToplevel source 
  case parsedSrc of
    Left err -> do
      case processOptions of
        CliOptions {failOnErrors = True} -> error $ show err
        _ -> print err >> return Nothing
    Right expressions -> do
      (res, defs) <- genModule oldDefs expressions processOptions
      return $ Just (res, defs)
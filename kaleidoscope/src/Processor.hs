module Processor where

import qualified LLVM.AST as AST (Definition) 
import ParserH (parseToplevel)
import CodeGen.GenModule (genModule)
import CLIParameters (CLIParameters(failOnErrors, CLIParameters))

process :: [AST.Definition] -> String -> CLIParameters -> IO (Maybe (String, [AST.Definition]))
process oldDefs source cliParameters = do
  let parsedSrc = parseToplevel source 
  case parsedSrc of
    Left err -> do
      case cliParameters of
        CLIParameters {failOnErrors = True} -> error $ show err
        _ -> print err >> return Nothing
    Right expressions -> do
      (res, defs) <- genModule oldDefs expressions cliParameters
      return $ Just (res, defs)
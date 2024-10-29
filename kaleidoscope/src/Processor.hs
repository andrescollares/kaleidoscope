module Processor where

import CLIParameters (CLIParameters (CLIParameters, failOnErrors))
import CodeGen.GenModule (genModule)
import qualified LLVM.AST as AST (Definition)
import Parser.Parse (parseToplevel)

process :: [AST.Definition] -> String -> CLIParameters -> IO (Maybe (String, [AST.Definition]))
process oldDefs newSource cliParameters = do
  let parsedSrc = parseToplevel newSource
  case parsedSrc of
    Left err -> do
      case cliParameters of
        CLIParameters {failOnErrors = True} -> error $ show err
        _ -> print err >> return Nothing
    Right expressions -> do
      (res, defs) <- genModule oldDefs expressions cliParameters
      return $ Just (res, defs)
{-# LANGUAGE OverloadedStrings #-}

module Processor where

import CLIParameters (CLIParameters (CLIParameters, failOnErrors))
import CodeGen.GenModule (genModule)
import qualified LLVM.AST as AST (Definition)
import Parser.Parse (parseToplevel)
import LLVM.AST (Module(moduleName, moduleDefinitions), defaultModule)
import CodeGen.JIT (optimizeModule, runJIT)

process :: [AST.Definition] -> String -> CLIParameters -> IO (Maybe [AST.Definition])
process oldDefs newSource cliParameters = do
  let parsedSrc = parseToplevel newSource
  case parsedSrc of
    Left err -> do
      case cliParameters of
        CLIParameters {failOnErrors = True} -> error $ show err
        _ -> print err >> return Nothing
    Right expressions -> do
      let defs = genModule oldDefs expressions

      -- Create module, compile it and execute it using the JIT
      let newModule = defaultModule {moduleName = "kaleidoscope", moduleDefinitions = defs}
      optimizedModule <- optimizeModule newModule cliParameters
      _ <- runJIT optimizedModule

      return $ Just defs

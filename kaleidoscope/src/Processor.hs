{-# LANGUAGE OverloadedStrings #-}

module Processor where

import CLIParameters (CLIParameters (CLIParameters, compile, emitAST, failOnErrors))
import CodeGen.GenModule (genModule)
import CodeGen.JIT (optimizeModule, runJIT)
import LLVM.AST (Module (moduleDefinitions, moduleName), defaultModule)
import qualified LLVM.AST as AST (Definition)
import Parser.Parse (parseToplevel)

process :: [AST.Definition] -> String -> CLIParameters -> IO (Maybe [AST.Definition])
process oldDefs newSource cliParameters = do
  let parsedSrc = parseToplevel newSource
  case parsedSrc of
    Left err -> do
      if failOnErrorsEnabled
        then error $ show err
        else print err >> return Nothing
    Right expressions -> do
      let defs = genModule oldDefs expressions
      if emitASTEnabled
        then putStrLn $ "Parsed expressions: " ++ show expressions
        else return ()

      -- Create module, compile it and execute it using the JIT
      let newModule = defaultModule {moduleName = "kaleidoscope", moduleDefinitions = defs}
      optimizedModule <- optimizeModule newModule cliParameters
      if compileEnabled
        then return $ Just defs
        else do
          _ <- runJIT optimizedModule
          return $ Just defs
  where
    CLIParameters {failOnErrors = failOnErrorsEnabled, compile = compileEnabled, emitAST = emitASTEnabled} = cliParameters

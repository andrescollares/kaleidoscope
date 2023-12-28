{-# LANGUAGE OverloadedStrings #-}

module JIT where

import qualified Data.ByteString as BS
import Foreign.Ptr (FunPtr, castFunPtr)
import qualified LLVM.AST as AST
import LLVM.Context
import qualified LLVM.ExecutionEngine as EE
import LLVM.Module as Mod
import LLVM.PassManager

foreign import ccall "dynamic" haskFun :: FunPtr Double -> Double

run :: FunPtr a -> Double
run fn = haskFun (castFunPtr fn :: FunPtr Double)

jit :: Context -> (EE.MCJIT -> IO a) -> IO a
jit c = EE.withMCJIT c optlevel model ptrelim fastins
  where
    optlevel = Just 3 -- optimization level
    model = Nothing -- code model ( Default )
    ptrelim = Nothing -- frame pointer elimination
    fastins = Nothing -- fast instruction selection

passes :: PassSetSpec
passes = defaultCuratedPassSetSpec {optLevel = Just 3}

optimizeModule :: AST.Module -> IO AST.Module
optimizeModule mod = do
  withContext $ \context ->
    jit context $ \_ ->
      withModuleFromAST context mod $ \m ->
        withPassManager passes $ \pm -> do
          -- Optimization Pass
          _ <- runPassManager pm m
          optmod <- moduleAST m
          modBS <- moduleLLVMAssembly m
          -- Print the optimized module as LLVM assembly to stdout
          putStrLn "Optimized LLVM assembly:"
          putStrLn $ modBSToString modBS
          -- Return the optimized module
          return optmod
  where
    modBSToString modBS = map (toEnum . fromIntegral) (BS.unpack modBS)

runJIT :: AST.Module -> IO Double
runJIT mod = do
  withContext $ \context ->
    jit context $ \executionEngine ->
      withModuleFromAST context mod $ \m ->
        EE.withModuleInEngine executionEngine m $ \ee -> do
          mainfn <- EE.getFunction ee (AST.Name "main")
          case mainfn of
            Just fn -> do
              putStrLn $ "Evaluated to: " ++ show result
              return result
              where
                result = run fn
            Nothing -> return 0

{-# LANGUAGE OverloadedStrings #-}

module JIT where

import qualified Data.ByteString as BS
import Foreign.C.Types
import Foreign.Ptr (FunPtr, castFunPtr)
import qualified LLVM.AST as AST
import qualified LLVM.AST.Type as ASTType
import LLVM.Context
import qualified LLVM.ExecutionEngine as EE
import LLVM.Module as Mod
import LLVM.PassManager

foreign import ccall "dynamic" haskFun :: FunPtr Double -> Double

foreign import ccall "dynamic" haskFunInt :: FunPtr CInt -> CInt

run :: FunPtr a -> Double
run fn = haskFun (castFunPtr fn :: FunPtr Double)

runInteger :: FunPtr a -> CInt
runInteger fn = haskFunInt (castFunPtr fn :: FunPtr CInt)

jit :: Context -> (EE.MCJIT -> IO a) -> IO a
jit c = EE.withMCJIT c optlevel model ptrelim fastins
  where
    optlevel = Just 0 -- optimization level
    model = Nothing -- code model ( Default )
    ptrelim = Nothing -- frame pointer elimination
    fastins = Nothing -- fast instruction selection

passes :: PassSetSpec
passes = defaultCuratedPassSetSpec {optLevel = Just 3}

optimizeModule :: AST.Module -> IO AST.Module
optimizeModule astModule = do
  withContext $ \context ->
    jit context $ \_ ->
      withModuleFromAST context astModule $ \m ->
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

runJIT :: AST.Module -> AST.Type -> IO Double
runJIT astModule runType = do
  withContext $ \context ->
    jit context $ \executionEngine ->
      withModuleFromAST context astModule $ \m ->
        EE.withModuleInEngine executionEngine m $ \ee -> do
          mainfn <- EE.getFunction ee (AST.Name "main")
          case mainfn of
            Just fn -> do
              putStrLn $ "Evaluated to: " ++ show result
              return result
              where
                result = case runType of
                  ASTType.FloatingPointType ASTType.DoubleFP -> run fn
                  _ -> fromIntegral $ runInteger fn
            Nothing -> return 0

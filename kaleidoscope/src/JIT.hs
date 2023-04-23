{-# LANGUAGE OverloadedStrings #-}

module JIT where

import Data.Int
import Data.Word
import Foreign.Ptr ( FunPtr, castFunPtr )

import Control.Monad.Except
import Control.Monad.State
import Control.Monad.Trans (lift)
import Data.ByteString.Short
import Data.String

import LLVM.Target
import LLVM.Context
import LLVM.CodeModel
import LLVM.Module as Mod
import qualified LLVM.AST as AST

import LLVM.PassManager
import LLVM.Transforms
import LLVM.Analysis

import qualified LLVM.ExecutionEngine as EE
import qualified Data.ByteString as BS

foreign import ccall "dynamic" haskFun :: FunPtr (IO Double) -> (IO Double)

run :: FunPtr a -> IO Double
run fn = haskFun (castFunPtr fn :: FunPtr (IO Double))

jit :: Context -> (EE.MCJIT -> IO a) -> IO a
jit c = EE.withMCJIT c optlevel model ptrelim fastins
  where
    optlevel = Just 0  -- optimization level
    model    = Nothing -- code model ( Default )
    ptrelim  = Nothing -- frame pointer elimination
    fastins  = Nothing -- fast instruction selection

passes :: PassSetSpec
passes = defaultCuratedPassSetSpec { optLevel = Just 3 }

type MainFuncState = Int

increment :: StateT MainFuncState IO ()
increment = modify (+1)

getMainCount :: StateT MainFuncState IO MainFuncState
getMainCount = get

advanceMain :: StateT MainFuncState IO MainFuncState
advanceMain = do
  modify (+1)
  current <- get
  return current

runJIT :: AST.Module -> IO (AST.Module)
runJIT mod = do
  let initialCount = 0
  withContext $ \context ->
    jit context $ \executionEngine ->
      withModuleFromAST context mod $ \m ->
        withPassManager passes $ \pm -> do
          -- Optimization Pass
          -- runPassManager pm m
          optmod <- moduleAST m
          s <- moduleLLVMAssembly m
          putStrLn $ map (toEnum . fromIntegral) (BS.unpack s)

          EE.withModuleInEngine executionEngine m $ \ee -> do
            mainCount <- execStateT advanceMain initialCount
            mainfn <- EE.getFunction ee (AST.Name $ fromString $ "main" ++ (show mainCount))
            print mainCount
            -- let mainName = case currentMain of
            --   0 -> "main"
            --   _ -> "main"
            case mainfn of
              Just fn -> do
                res <- run fn
                putStrLn $ "Evaluated to: " ++ show res
              Nothing -> return ()

          -- Return the optimized module
          return optmod
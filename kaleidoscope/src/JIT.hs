{-# LANGUAGE OverloadedStrings #-}

module JIT where

import qualified Data.ByteString as BS
import Foreign.C.Types ( CInt(..), CBool(..) )
import Foreign.Ptr (FunPtr, castFunPtr)
import qualified LLVM.AST as AST
import qualified LLVM.AST.Type as ASTType
import LLVM.AST (Operand (ConstantOperand, LocalReference, MetadataOperand), Type (IntegerType, FloatingPointType))
import LLVM.Context ( withContext, Context )
import qualified LLVM.ExecutionEngine as EE
import LLVM.Module as Mod
    ( moduleAST, moduleLLVMAssembly, withModuleFromAST )
import LLVM.PassManager
    ( defaultCuratedPassSetSpec,
      runPassManager,
      withPassManager,
      PassSetSpec(optLevel) )
import Data.ByteString.Short (ShortByteString)
import LLVM.AST.Constant (Constant(integerBits))

foreign import ccall "dynamic" haskFunDouble :: FunPtr Double -> Double

foreign import ccall "dynamic" haskFunInt :: FunPtr CInt -> CInt

foreign import ccall "dynamic" haskFunBool :: FunPtr CBool -> CBool

runDouble :: FunPtr a -> Double
runDouble fn = haskFunDouble (castFunPtr fn :: FunPtr Double)

runInteger :: FunPtr a -> CInt
runInteger fn = haskFunInt (castFunPtr fn :: FunPtr CInt)

runBool :: FunPtr a -> CInt
runBool fn = haskFunInt (castFunPtr fn :: FunPtr CInt)

jit :: Context -> (EE.MCJIT -> IO a) -> IO a
jit c = EE.withMCJIT c optlevel model ptrelim fastins
  where
    optlevel = Just 0 -- optimization level
    model = Nothing -- code model ( Default )
    ptrelim = Nothing -- frame pointer elimination
    fastins = Nothing -- fast instruction selection

passes :: Word -> PassSetSpec
passes level = defaultCuratedPassSetSpec {optLevel = Just level}

optimizeModule :: AST.Module -> Word -> IO AST.Module
optimizeModule astModule level = do
  withContext $ \context ->
    jit context $ \_ ->
      withModuleFromAST context astModule $ \m ->
        withPassManager (passes level) $ \pm -> do
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

runJIT :: AST.Module -> AST.Type -> IO String
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
                  FloatingPointType _ -> show $ runDouble fn
                  IntegerType { ASTType.typeBits = 32 } -> show $ runInteger fn
                  IntegerType { ASTType.typeBits = 1 } -> if runBool fn == 0 then "false" else "true"
                  _ -> error "Unknown expression type"
            Nothing -> return "0"

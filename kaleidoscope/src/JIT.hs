{-# LANGUAGE OverloadedStrings #-}

module JIT where

import qualified Data.ByteString as BS
import Foreign.C.Types ( CInt(..), CBool(..) )
import Foreign.Ptr (FunPtr, castFunPtr)
import qualified LLVM.AST as AST
import qualified LLVM.AST.Type as ASTType
import LLVM.AST (Type (IntegerType, FloatingPointType, StructureType))
import LLVM.Context ( withContext, Context )
import qualified LLVM.ExecutionEngine as EE
import LLVM.Module as Mod
    ( moduleAST, moduleLLVMAssembly, withModuleFromAST )
import LLVM.PassManager
    ( defaultCuratedPassSetSpec,
      runPassManager,
      withPassManager,
      PassSetSpec(optLevel) )
import CLI (CliOptions (CliOptions, optimizationLevel, emitLLVM))
import Data.String ( IsString(fromString) )

foreign import ccall "dynamic" haskFunDouble :: FunPtr Double -> Double

foreign import ccall "dynamic" haskFunInt :: FunPtr CInt -> CInt

runDouble :: FunPtr a -> Double
runDouble fn = haskFunDouble (castFunPtr fn :: FunPtr Double)

runInteger :: FunPtr a -> CInt
runInteger fn = haskFunInt (castFunPtr fn :: FunPtr CInt)

runBool :: FunPtr a -> CInt
runBool fn = haskFunInt (castFunPtr fn :: FunPtr CInt)

jit :: Context -> Word -> (EE.MCJIT -> IO a) -> IO a
jit c oLevel = EE.withMCJIT c optlevel model ptrelim fastins
  where
    optlevel = Just oLevel -- optimization level
    model = Nothing -- code model ( Default )
    ptrelim = Nothing -- frame pointer elimination
    fastins = Nothing -- fast instruction selection

passes :: Word -> PassSetSpec
passes level = defaultCuratedPassSetSpec {optLevel = Just level}

optimizeModule :: AST.Module -> CliOptions -> IO AST.Module
optimizeModule astModule CliOptions { optimizationLevel = level, emitLLVM = emit } = do
  withContext $ \context ->
    jit context level $ \_ ->
      withModuleFromAST context astModule $ \m ->
        withPassManager (passes level) $ \pm -> do
          -- Optimization Pass
          _ <- runPassManager pm m
          optmod <- moduleAST m
          if emit then (do
            modBS <- moduleLLVMAssembly m
            -- Print the optimized module as LLVM assembly to stdout
            putStrLn "Optimized LLVM assembly:"
            putStrLn $ modBSToString modBS
            -- Return the optimized module
            return optmod)
          else return optmod
  where
    modBSToString modBS = map (toEnum . fromIntegral) (BS.unpack modBS)

runJIT :: AST.Module -> AST.Type -> IO String
runJIT astModule runType = do
  withContext $ \context ->
    jit context 0 $ \executionEngine ->
      withModuleFromAST context astModule $ \m ->
        EE.withModuleInEngine executionEngine m $ \ee -> do
          printer <- EE.getFunction ee (AST.Name $ fromString $ printerFunctionName runType)
          putStrLn "Running JIT..."
          putStrLn $ result printer
          return "Done!"
          where
            result p = case p of
              Just fn -> "Evaluated to" ++ show (runInteger fn)
              Nothing -> error "Unable to print this result"


            --   where
            --     result = case runType of
            --       FloatingPointType _ -> show $ runDouble fn
            --       IntegerType { ASTType.typeBits = 32 } -> show $ runInteger fn
            --       IntegerType { ASTType.typeBits = 1 } -> if runBool fn == 0 then "false" else "true"
            --       -- TODO: find a better way to print the tuples
            --       StructureType { ASTType.elementTypes = [t1, t2] } -> "Tuple (" ++ firstElem ++ ", ...)"
            --         where firstElem = case t1 of
            --                 IntegerType { ASTType.typeBits = 32 } -> show $ runInteger fn -- FIXME: runInteger can't handle lists longer that 3 :skull_emoji:
            --                 IntegerType { ASTType.typeBits = 1 } -> if runBool
              --  fn == 0 then "false" else "true"
            --                 FloatingPointType _ -> show $ runDouble fn
            --                 _ -> error "Unknown expression type"
            --       _ -> error "Unknown expression type"
            -- Nothing -> return "0"

printerFunctionName :: AST.Type -> String
printerFunctionName (FloatingPointType _) = "aasdasd"
printerFunctionName (IntegerType { ASTType.typeBits = 32 }) = "printIntMainFunction"
printerFunctionName (IntegerType { ASTType.typeBits = 1 }) = "asdasd"
printerFunctionName _ = error "Unable to print this result"


{-# LANGUAGE OverloadedStrings #-}

module CodeGen.JIT where

import CLIParameters (CLIParameters (CLIParameters, compile, emitLLVM, inputFile, optimizationLevel))
import qualified Data.ByteString as BS
import Foreign.C.Types (CInt (..))
import Foreign.Ptr (FunPtr, castFunPtr)
import qualified LLVM.AST as AST
import LLVM.Context (Context, withContext)
import qualified LLVM.ExecutionEngine as EE
import LLVM.Module as Mod
  ( moduleAST,
    moduleLLVMAssembly,
    withModuleFromAST,
  )
import LLVM.PassManager
  ( PassSetSpec (optLevel),
    defaultCuratedPassSetSpec,
    runPassManager,
    withPassManager,
  )
import System.Process (system)

foreign import ccall "dynamic" haskFunInt :: FunPtr CInt -> CInt

runInteger :: FunPtr a -> CInt
runInteger fn = haskFunInt (castFunPtr fn :: FunPtr CInt)

jit :: Context -> Word -> (EE.MCJIT -> IO a) -> IO a
jit c oLevel = EE.withMCJIT c optlevel model ptrelim fastins
  where
    optlevel = Just oLevel -- optimization level
    model = Nothing -- code model ( Default )
    ptrelim = Nothing -- frame pointer elimination
    fastins = Nothing -- fast instruction selection

optimizeModule :: AST.Module -> CLIParameters -> IO AST.Module
optimizeModule astModule CLIParameters {optimizationLevel = level, emitLLVM = emit, compile = compileFile, inputFile = file} = do
  withContext $ \context ->
    jit context level $ \_ ->
      withModuleFromAST context astModule $ \m ->
        withPassManager (defaultCuratedPassSetSpec {optLevel = Just level}) $ \pm -> do
          -- Optimization Pass
          _ <- runPassManager pm m
          optmod <- moduleAST m
          if emit && not compileFile
            then do
              modByteString <- moduleLLVMAssembly m
              -- Print the optimized module as LLVM assembly to stdout
              putStrLn "Optimized LLVM assembly:"
              putStrLn $ modBSToString modByteString
              -- Return the optimized module
              return optmod
            else
              if compileFile && file /= ""
                then do
                  let llvmSrc = fileNameLLVM file
                  let objectFile = fileWithoutExtension file
                  modByteString <- moduleLLVMAssembly m
                  writeLLVM (modBSToString modByteString) llvmSrc
                  compileLLVMWithClang llvmSrc objectFile
                  return optmod
                else return optmod
  where
    modBSToString modByteString = map (toEnum . fromIntegral) (BS.unpack modByteString)

fileNameLLVM :: String -> String
fileNameLLVM file = fileWithoutExtension file ++ ".ll"

fileWithoutExtension :: String -> String
fileWithoutExtension = reverse . tail . dropWhile (/= '.') . reverse

writeLLVM :: String -> String -> IO ()
writeLLVM moduleSrc fileName = do
  writeFile fileName moduleSrc

-- usage: cabal run kaleidoscope-fing -- -f ./test/programs/add_int_int.k -c
-- clang /kaleidoscope/out.ll -o my_program -L/usr/lib -lstdlib -o outprogram
-- ./outprogram.o
compileLLVMWithClang :: String -> String -> IO ()
compileLLVMWithClang fileNameLlvm fileNameExecutable = do
  _ <- system cmd
  putStrLn cmd
  return ()
  where
    cmd = "clang " ++ fileNameLlvm ++ " -o " ++ fileNameExecutable ++ " " ++ linkedLibraryDirectory ++ " " ++ linkedLibraryName
    linkedLibraryDirectory = "-L/usr/lib"
    linkedLibraryName = "-lstdlib" -- the actual name of the library should be libstdlib.so

runJIT :: AST.Module -> IO String
runJIT astModule = do
  withContext $ \context ->
    jit context 0 $ \executionEngine ->
      withModuleFromAST context astModule $ \m ->
        EE.withModuleInEngine executionEngine m $ \ee -> do
          mainFn <- EE.getFunction ee "main"
          case mainFn of
            Just fn -> do
              putStr $ if show (runInteger fn) == "" then "Error" else "\n"
              return $ show (runInteger fn)
            Nothing -> return "0"

{-# LANGUAGE OverloadedStrings #-}

module Emit where

import Codegen
import Control.Monad.Except
import Data.ByteString.Short
import qualified Data.Map as Map
import Data.String
import JIT
import qualified LLVM.AST as AST
import qualified LLVM.AST.Constant as C
import qualified LLVM.AST.Float as F
import qualified LLVM.AST.FloatingPointPredicate as FP
import qualified Syntax as S

toSig :: [ShortByteString] -> [(AST.Type, AST.Name)]
toSig = map (\x -> (double, AST.Name x))

codegenTop :: S.Expr -> LLVM ()
codegenTop (S.Function name arguments body) = do
  define double (fromString name) fnargs bls
  where
    fnargs = toSig (map fromString arguments)
    bls = createBlocks $
      execCodegen $ do
        entryBlk <- addBlock entryBlockName
        _ <- setBlock entryBlk
        forM_ arguments $ \a -> do
          var <- alloca double
          store var (local (AST.Name $ fromString a))
          assign (fromString a) var
        cgen body >>= ret
codegenTop (S.Extern name arguments) = do
  external double (fromString name) fnargs
  where
    fnargs = toSig $ map fromString arguments
codegenTop expression = do
  define double "main" [] blks
  where
    blks = createBlocks $
      execCodegen $ do
        entryBlk <- addBlock entryBlockName
        _ <- setBlock entryBlk
        cgen expression >>= ret

-------------------------------------------------------------------------------
-- Operations
-------------------------------------------------------------------------------

lt :: AST.Operand -> AST.Operand -> Codegen AST.Operand
lt a b = do
  test <- fcmp FP.ULT a b
  uitofp double test

gt :: AST.Operand -> AST.Operand -> Codegen AST.Operand
gt a b = do
  test <- fcmp FP.UGT a b
  uitofp double test

binops :: Map.Map String (AST.Operand -> AST.Operand -> Codegen AST.Operand)
binops =
  Map.fromList
    [ ("+", fadd),
      ("-", fsub),
      ("*", fmul),
      ("/", fdiv),
      ("<", lt),
      (">", gt)
    ]

cgen :: S.Expr -> Codegen AST.Operand
cgen (S.UnaryOp op a) = do
  cgen $ S.Call ("unary" ++ op) [a]
cgen (S.BinOp "=" (S.Var var) val) = do
  a <- getvar (fromString var)
  cval <- cgen val
  store a cval
  return cval
cgen (S.BinOp op a b) = do
  case Map.lookup op binops of
    Just f -> do
      ca <- cgen a
      cb <- cgen b
      f ca cb
    Nothing -> error "No such operator"
cgen (S.If cond thenExpr elseExpr) = do
  ifthen <- addBlock "if.then"
  ifelse <- addBlock "if.else"
  ifcont <- addBlock "if.cont"

  -- %entry
  ccond <- cgen cond
  test <- fcmp FP.ONE (cons $ C.Float (F.Double 0.0)) ccond
  _ <- cbr test ifthen ifelse

  -- if.then
  _ <- setBlock ifthen
  thenval <- cgen thenExpr
  _ <- br ifcont
  ifthenCode <- getBlock

  -- if.else
  _ <- setBlock ifelse
  elseval <- cgen elseExpr
  _ <- br ifcont
  ifelseCode <- getBlock

  -- if.cont
  _ <- setBlock ifcont
  phi double [(thenval, ifthenCode), (elseval, ifelseCode)]
cgen (S.Var x) = getvar (fromString x) >>= load
cgen (S.Float n) = return $ cons $ C.Float (F.Double n)
cgen (S.Call fn args) = do
  largs <- mapM cgen args
  call (externf (AST.Name $ fromString fn) largs) largs
cgen _ = error "This shouldn't have matched here :thinking_emoji"

-------------------------------------------------------------------------------
-- Compilation
-------------------------------------------------------------------------------

liftError :: ExceptT String IO a -> IO a
liftError = runExceptT >=> either fail return

codegen :: AST.Module -> [S.Expr] -> IO AST.Module
codegen modl fns = do
  res <- runJIT oldAst
  return res
  where
    modlName = mapM codegenTop fns
    oldAst = runLLVM modl modlName

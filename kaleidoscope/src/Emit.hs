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

one = cons $ C.Float (F.Double 1.0)

zero = cons $ C.Float (F.Double 0.0)

false = zero

true = one

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
codegenTop (S.Constant name body) = do
  -- TODO: Override name/delete previous function if already exists
  define double (fromString name) [] bls
  where
    bls = createBlocks $
      execCodegen $ do
        entryBlk <- addBlock entryBlockName
        _ <- setBlock entryBlk
        cgen body >>= ret
codegenTop (S.Extern name arguments) = do
  external double (fromString name) fnargs
  where
    fnargs = toSig $ map fromString arguments
codegenTop (S.BinaryDef name args body) =
  codegenTop $ S.Function ("binary" ++ name) args body
codegenTop (S.UnaryDef name args body) =
  codegenTop $ S.Function ("unary" ++ name) args body
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
-- cgen (S.BinOp "=" (S.Var var) val) = do
--   cval <- cgen val
--   a <- getOrAssignVar (fromString var) cval
--   store a cval
--   return cval
cgen (S.BinOp op a b) = do
  case Map.lookup op binops of
    Just f -> do
      ca <- cgen a
      cb <- cgen b
      f ca cb
    Nothing -> cgen (S.Call ("binary" ++ op) [a, b])
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
cgen (S.For ivar start cond step body) = do
  forloop <- addBlock "for.loop"
  forexit <- addBlock "for.exit"

  -- %entry
  ------------------
  i <- alloca double
  istart <- cgen start -- Generate loop variable initial value
  stepval <- cgen step -- Generate loop variable step
  store i istart -- Store the loop variable initial value
  assign (fromString ivar) i -- Assign loop variable to the variable name
  br forloop -- Branch to the loop body block

  -- for.loop
  ------------------
  setBlock forloop
  cgen body -- Generate the loop body
  ival <- load i -- Load the current loop iteration
  inext <- fadd ival stepval -- Increment loop variable
  store i inext

  cond <- cgen cond -- Generate the loop condition
  test <- fcmp FP.ONE false cond -- Test if the loop condition is True ( 1.0 )
  cbr test forloop forexit -- Generate the loop condition

  -- for.exit
  ------------------
  setBlock forexit
  return zero
cgen (S.Let a b c) = do
  i <- alloca double
  val <- cgen b
  store i val
  assign (fromString a) i
  cgen c
cgen (S.Var x) = do
  maybeVal <- runMaybeT $ getvar (fromString x)
  case maybeVal of
    Just val -> load val
    Nothing -> cgen (S.Call x [])
cgen (S.Float n) = return $ cons $ C.Float (F.Double n)
cgen (S.Call fn args) = do
  largs <- mapM cgen args
  call (externf (AST.Name $ fromString fn) largs) largs
cgen _ = error "This shouldn't have matched here :thinking_emoji:"

-------------------------------------------------------------------------------
-- Compilation
-------------------------------------------------------------------------------

liftError :: ExceptT String IO a -> IO a
liftError = runExceptT >=> either fail return

codegen :: AST.Module -> [S.Expr] -> IO AST.Module
codegen modl expressions = do
  res <- optimizeModule oldAst
  runJIT oldAst
  return res
  where
    modlName = mapM codegenTop expressions
    oldAst = runLLVM modl modlName

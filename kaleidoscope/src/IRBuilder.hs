{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecursiveDo #-}

module IRBuilder where

import Data.ByteString.Short
import Debug.Trace
import JIT
import LLVM.AST as AST hiding (function)
import qualified LLVM.AST.Type as ASTType
import qualified LLVM.AST.Constant as C
import qualified LLVM.AST.Float as F
import qualified LLVM.AST.IntegerPredicate as P
import qualified LLVM.AST.ParameterAttribute as PA
import LLVM.IRBuilder.Constant as Con
import LLVM.IRBuilder.Instruction
import LLVM.IRBuilder.Module
import LLVM.IRBuilder.Monad
import Control.Monad (liftM)
import LLVM.AST.Constant (Constant(GlobalReference))
import Data.String
import Data.Maybe
import qualified Data.Map.Strict as M
-- import Control.Monad.RWS (MonadTrans, MonadState (state, get), MonadReader (local))
import Control.Monad.State.Strict
import Control.Monad.State.Class
import LLVM.AST.FloatingPointPredicate (FloatingPointPredicate(ULT, UGT, UEQ, UNE, ULE, UGE, ONE))

simple :: Module
simple = buildModule "exampleModule" $ do
  function "plus" [(ASTType.i32, "x"), (ASTType.i32, "y")] ASTType.i32 $ \[x, y] -> do
    r <- add x y
    ret r

conditional :: Module
conditional = buildModule "conditionModule" $ do
  -- This breaks the SSA Principle: https://stackoverflow.com/a/70901888
  -- f <- function "f" [(AST.i32, "a")] AST.i32 $ \[a] -> mdo
  --   cond <- icmp P.EQ a (ConstantOperand (C.Int 32 0))
  --   condBr cond ifThen ifElse
  --   ifThen <- block `named` "if.then"
  --   trVal <- add a (ConstantOperand (C.Int 32 0))
  --   ret trVal
  --   -- br ifExit
  --   ifElse <- block `named` "if.else"
  --   flVal <- add a (ConstantOperand (C.Int 32 0))
  --   ret flVal

  f <- function "f" [(ASTType.i32, "a")] ASTType.i32 $ \[a] -> mdo
    cond <- icmp P.EQ a (ConstantOperand (C.Int 32 0))
    condBr cond ifThen ifElse
    ifThen <- block `named` "if.then"
    trVal <- add a (ConstantOperand (C.Int 32 0))
    br ifExit
    ifElse <- block `named` "if.else"
    flVal <- add a (ConstantOperand (C.Int 32 0))
    br ifExit
    ifExit <- block `named` "if.exit"
    -- SSA
    r <- phi [(trVal, ifThen), (flVal, ifElse)]
    ret r

  function "main" [] ASTType.i32 $ \[] -> mdo
    -- the empty array are the parameter attributes: https://hackage.haskell.org/package/llvm-hs-pure-9.0.0/docs/LLVM-ASTType.ParameterAttribute.html
    r <- call f [(ConstantOperand (C.Int 32 0), [])]
    ret r

arithmetrics :: Module
arithmetrics = buildModule "arithmetrics" $ do
  -- function "+" [(ASTType.i32, "x"), (ASTType.i32, "y")] ASTType.i32 $ \[x, y] -> do
  --   r <- add x y
  --   ret r

  -- function "-" [(ASTType.i32, "x"), (ASTType.i32, "y")] ASTType.i32 $ \[x, y] -> do
  --   r <- sub x y
  --   ret r

  -- function "*" [(ASTType.i32, "x"), (ASTType.i32, "y")] ASTType.i32 $ \[x, y] -> do
  --   r <- mul x y
  --   ret r

  -- function "/" [(ASTType.i32, "x"), (ASTType.i32, "y")] ASTType.i32 $ \[x, y] -> do
  --   r <- sdiv x y
  --   ret r

  function "%" [(ASTType.i32, "x"), (ASTType.i32, "y")] ASTType.i32 $ \[x, y] -> do
    _ <- trace (show x) $ pure ()

    var <- fresh `named` "x"
    r <- srem (LocalReference ASTType.i32 var) y
    ret r

globalDef :: Module
globalDef = buildModule "variable_test" $ do
  x <- global "x" ASTType.double (C.Float (F.Double 50.0))

  function "main" [] ASTType.double $ \[] -> do
    x1 <- load (ConstantOperand (GlobalReference (ASTType.ptr ASTType.double) "y")) 0
    r <- fadd x1 (ConstantOperand (C.Float (F.Double 1.0)))
    -- res <- call (ConstantOperand (C.GlobalReference (ptr (FunctionType ASTType.i32 [ ] False)) (Name "f")) ) [ ]
    ret x1

genOptimizedMainModuleIR :: IO Module
genOptimizedMainModuleIR = genModule [Float 5.0]

genSimpleFunction :: IO Module
genSimpleFunction = genModule [IRBuilder.Function "plus" ["x", "y"] (Float 5.0)]

genNotSoSimpleFunction :: IO Module
genNotSoSimpleFunction = genModule [IRBuilder.Function "id" ["x"] (Var "x")]

genFunctionCall :: IO Module
genFunctionCall = genModule [ 
  IRBuilder.Function "one" [] (Float 1.0), 
  IRBuilder.Call "one" []
  ]

genIfFalse :: IO Module
genIfFalse = genModule [IRBuilder.If (BinOp ">" (Float 0.0) (Float 1.0)) (Float 8.0) (Float 2.0)]

genIfTrue :: IO Module
genIfTrue = genModule [IRBuilder.If (Float 1.0) (Float 5.0) (Float 2.0)]

genRecursive :: IO Module
genRecursive = genModule [
  IRBuilder.Function "rec" ["x", "y"] (If (BinOp "<" (Var "x") (Float 1.0)) (Var "y") (IRBuilder.Call "rec" [BinOp "-" (Var "x") (Float 1.0), BinOp "+" (Var "y") (Var "x")])),
  IRBuilder.Call "rec" [Float 5.0, Float 0.0]
  ]

-- Syntax
data Expr
  = Float Double
  | Let Name Expr Expr
  | Var Name
  | Call Name [Expr]
  | Function Name [ParameterName] Expr
  | Extern Name [Name]
  | UnaryOp ShortByteString Expr
  | BinOp ShortByteString Expr Expr
  | If Expr Expr Expr
  deriving stock (Eq, Ord, Show)

-- Generates the Module from the previous module and the new expressions
-- Has to optimize the module
-- Has to execute the module
-- Has to update the module state
genModule :: [Expr] -> IO Module
genModule expressions = do
  res <- optimizeModule unoptimizedAst
  runJIT res
  return res
  where
    -- use old state and new expressions to generate the new state
    modlState = mapM genTopLevel expressions
    unoptimizedAst = buildModule "kaleidoscope" modlState

-- Generates functions, constants, externs, definitions and a main function otherwise
-- The result is a ModuleBuilder monad
genTopLevel :: Expr -> ModuleBuilder Operand
genTopLevel (IRBuilder.Function name args body) = do
  function name (map (\x -> (ASTType.double, x)) args) ASTType.double (genLevel body)
genTopLevel (IRBuilder.Extern name args) = do
  extern name (map (const ASTType.double) args) ASTType.double
genTopLevel expression = do
  function "main" [] ASTType.double (genLevel expression)

genLevel :: Expr -> [Operand] -> IRBuilderT ModuleBuilder ()
genLevel e localVars = genOperand e localVars >>= ret

-- Generates the Operands that codegenTop needs.
genOperand :: Expr -> [Operand] -> IRBuilderT ModuleBuilder Operand
genOperand (Float n) _ = return $ ConstantOperand (C.Float (F.Double n))

genOperand (IRBuilder.Call fn args) localVars = do
  largs <- mapM (`genOperand` localVars) args 
  call (ConstantOperand (C.GlobalReference (ASTType.ptr (FunctionType ASTType.double (map (const ASTType.double) args) False)) fn)) (map (\x -> (x, [])) largs)

genOperand (UnaryOp oper a) localVars = do
  op <- genOperand a localVars
  case M.lookup oper unops of
    Just f -> f op
    Nothing -> error "This shouldn't have matched here, unary operand doesn't exist."
  where
    unops :: M.Map ShortByteString (Operand -> IRBuilderT ModuleBuilder Operand)
    unops =
      M.fromList
        [ ("-", fneg) ]

genOperand (BinOp oper a b) localVars = do
  opA <- genOperand a localVars
  opB <- genOperand b localVars
  case M.lookup oper binops of
    Just f -> f opA opB
    Nothing -> error "This shouldn't have matched here, binary operand doesn't exist."
  where
    binops :: M.Map ShortByteString (Operand -> Operand -> IRBuilderT ModuleBuilder Operand)
    binops =
      M.fromList
        [ ("+", fadd),
          ("-", fsub),
          ("*", fmul),
          ("/", fdiv),
          ("<", fcmp ULT),
          (">", fcmp UGT),
          ("==", fcmp UEQ),
          ("!=", fcmp UNE),
          ("<=", fcmp ULE),
          (">=", fcmp UGE)
          ]
  
genOperand (If cond thenExpr elseExpr) localVars = mdo
  computedCond <- genOperand cond localVars
  -- test <- fcmp ONE computedCond (ConstantOperand (C.Float (F.Double 0.0)))
  condBr computedCond ifThen ifElse
  ifThen <- block `named` "if.then"
  computedThen <- genOperand thenExpr localVars
  br ifExit
  ifElse <- block `named` "if.else"
  computedElse <- genOperand elseExpr localVars
  br ifExit
  ifExit <- block `named` "if.exit"
  phi [(computedThen, ifThen), (computedElse, ifElse)]

genOperand (Let (Name varName) value body) localVars = do
  var <- alloca ASTType.double Nothing 0
  computedValue <- genOperand value localVars
  store var 0 computedValue
  genOperand body (var : localVars)

genOperand(Var (Name n)) localVars = do
  return $ LocalReference ASTType.double (Name $ n <> "_0") 

-- genOperand (Var (Name n)) localVars = do
--   s <- get 
--   let
--     usedNames = builderUsedNames s
--     nameCount = fromMaybe 0 $ M.lookup n usedNames
--     usedName = n <> fromString ("_" <> show (nameCount - 1))
--   case getLocalVar usedName localVars of
--     Just x -> return x
--     Nothing -> return $ ConstantOperand (GlobalReference (ASTType.ptr ASTType.double) (Name n))
--   where
--     getLocalVar :: ShortByteString -> [Operand] -> Maybe Operand
--     getLocalVar n vars = case filter (\(LocalReference _ (Name name)) -> name == n) vars of
--       x:xs -> Just x
--       _ -> Nothing

genOperand _ _ = error "This shouldn't have matched here :thinking_emoji:"
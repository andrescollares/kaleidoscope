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
import Control.Monad.RWS (gets, MonadTrans, MonadState (state), MonadReader (local))
import LLVM.AST.FloatingPointPredicate (FloatingPointPredicate(ULT, UGT))

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

-- Syntax
data Expr
  = Float Double
  | Var Name
  | Call Name [Expr]
  | Function Name [ParameterName] Expr
  | Extern Name [Name]
  | UnaryOp Name Expr
  | BinOp Name Expr Expr
  | If Expr Expr Expr
  | Let Name Expr Expr
  | For Name Expr Expr Expr Expr
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

-- genOperand (Var (Name n)) localVars = ret $ do
--   usedNames <- liftIRState gets builderUsedNames
--   let
--     nameCount = fromMaybe 0 $ M.lookup n usedNames
--     usedName = n <> fromString ("_" <> show (nameCount - 1))
--   case getLocalVar usedName localVars of
--     Just x -> ret x
--     Nothing -> ret $ ConstantOperand (GlobalReference (ASTType.ptr ASTType.double) (Name n))
--   where
--     getLocalVar :: ShortByteString -> [Operand] -> Maybe Operand
--     getLocalVar n vars = case filter (\(LocalReference _ (Name name)) -> name == n) vars of
--       x:xs -> Just x
--       _ -> Nothing

-- genOperand (UnaryOp (Name op) a) localVars = do
--   operand <- genOperand a localVars
--   genOperand (IRBuilder.Call (Name ("unary_" <> op)) [operand]) localVars

-- genOperand (BinOp (Name op) a b) localVars = do
--   opA <- genOperand a localVars
--   res <- case M.lookup op binops of
--     Just f -> f () (genOperand b localVars)
--     Nothing -> error "This shouldn't have matched here, binary operand doesn't exist."
--   ret res
--   -- genOperand (IRBuilder.Call (Name ("binary_" <> op)) [a, b]) localVars
--   where
--     binops :: M.Map ShortByteString (Operand -> Operand -> IRBuilderT ModuleBuilder Operand)
--     binops =
--       M.fromList
--         [ ("+", fadd),
--           ("-", fsub),
--           ("*", fmul),
--           ("/", fdiv),
--           ("<", fcmp ULT),
--           (">", fcmp UGT)
--         ]

genOperand _ _ = error "This shouldn't have matched here :thinking_emoji:"
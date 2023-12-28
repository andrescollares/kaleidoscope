{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecursiveDo #-}

module IRBuilder where

import Data.ByteString.Short
import qualified Data.Map.Strict as M
-- import Data.Maybe
import Data.String
import JIT
import LLVM.AST as AST hiding (function)
import qualified LLVM.AST.Constant as C
import qualified LLVM.AST.Float as F
import LLVM.AST.FloatingPointPredicate (FloatingPointPredicate (UEQ, UGE, UGT, ULE, ULT, UNE))
import LLVM.AST.Global (Global (name))
import qualified LLVM.AST.Type as ASTType
import LLVM.IRBuilder.Instruction
import LLVM.IRBuilder.Internal.SnocList
import LLVM.IRBuilder.Module (ModuleBuilder, ModuleBuilderState (ModuleBuilderState, builderDefs, builderTypeDefs), execModuleBuilder, extern, function, global)
import LLVM.IRBuilder.Monad
import Syntax as S

-- Generates the Module from the previous module and the new expressions
-- Has to optimize the module
-- Has to execute the module
-- Has to update the module state
genModule :: [Definition] -> [Expr] -> IO (Double, [Definition])
genModule oldDefs expressions = do
  optMod <- optimizeModule unoptimizedAst
  res <- runJIT optMod
  return (res, definitions)
  where
    -- TODO: Remove old duplicate functions
    -- use old state and new expressions to generate the new state
    modlState = mapM genTopLevel expressions
    oldDefsWithoutMain =
      filterFst
        ( \case
            GlobalDefinition AST.Function {name = Name "main"} -> True
            _ -> False
        )
        oldDefs
    filterFst _ [] = []
    filterFst p (x : xs)
      | p x = xs
      | otherwise = x : filterFst p xs

    definitions = buildModuleWithDefinitions oldDefsWithoutMain modlState
    unoptimizedAst = mkModule definitions
    mkModule ds = defaultModule {moduleName = "kaleidoscope", moduleDefinitions = ds}

buildModuleWithDefinitions :: [Definition] -> ModuleBuilder a -> [Definition]
buildModuleWithDefinitions prevDefs = execModuleBuilder oldModl
  where
    oldModl = ModuleBuilderState {builderDefs = SnocList (reverse prevDefs), builderTypeDefs = mempty}

-- Generates functions, constants, externs, definitions and a main function otherwise
-- The result is a ModuleBuilder monad
genTopLevel :: Expr -> ModuleBuilder Operand
-- Extern definition
genTopLevel (S.Extern name args) = do
  extern name (map (const ASTType.double) args) ASTType.double
-- Function definition
genTopLevel (S.Function name args body) = do
  function name (map (\x -> (ASTType.double, x)) args) ASTType.double (genLevel body)
-- Constant definition
genTopLevel (S.Constant name value) = do
  global name ASTType.double (C.Float (F.Double value))
-- Unary operator definition
genTopLevel (S.UnaryDef name args body) = do
  function (Name ("unary_" <> name)) (map (\x -> (ASTType.double, x)) args) ASTType.double (genLevel body)
-- Binary operator definition
genTopLevel (S.BinaryDef name args body) = do
  function (Name ("binary_" <> name)) (map (\x -> (ASTType.double, x)) args) ASTType.double (genLevel body)
-- Any expression
genTopLevel expression = do
  function "main" [] ASTType.double (genLevel expression)

genLevel :: Expr -> [Operand] -> IRBuilderT ModuleBuilder ()
genLevel e localVars = genOperand e localVars >>= ret

-- Generates the Operands that codegenTop needs.
genOperand :: Expr -> [Operand] -> IRBuilderT ModuleBuilder Operand
-- Float
genOperand (Float n) _ = return $ ConstantOperand (C.Float (F.Double n))
-- Variables
genOperand (Var (Name n)) localVars = do
  -- if localVars has it then it's a local reference otherwise mark it as a global reference
  -- local variable names end in "_number" so we need to take that into consideration
  -- also local variable names can have "_"
  case getLocalVarName n localVars of
    Just localVar -> return localVar
    Nothing -> load (ConstantOperand (C.GlobalReference (ASTType.ptr ASTType.double) (Name n))) 0
  where
    getLocalVarName :: ShortByteString -> [Operand] -> Maybe Operand
    getLocalVarName n vars = findLast (\(LocalReference _ (Name varName)) -> removeEnding varName == n) vars Nothing
    findLast :: (a -> Bool) -> [a] -> Maybe a -> Maybe a
    findLast p (x : xs) res
      | p x = findLast p xs (Just x)
      | otherwise = findLast p xs res
    findLast _ [] res = res
    -- TODO: Rework this function later, don't use show
    -- bytestring > 11.smth has implemented this function but llvm 12 doesn't permit bytestring > 11
    removeEnding :: ShortByteString -> ShortByteString
    removeEnding n = fromString $ tail $ reverse $ tail $ dropWhile (/= '_') (reverse $ show n)

-- Call
genOperand (S.Call fn args) localVars = do
  largs <- mapM (`genOperand` localVars) args
  call (ConstantOperand (C.GlobalReference (ASTType.ptr (FunctionType ASTType.double (map (const ASTType.double) args) False)) fn)) (map (\x -> (x, [])) largs)

-- Unary Operands
genOperand (UnaryOp oper a) localVars = do
  op <- genOperand a localVars
  case M.lookup oper unops of
    Just f -> f op
    Nothing -> error "This shouldn't have matched here, unary operand doesn't exist."
  where
    unops :: M.Map ShortByteString (Operand -> IRBuilderT ModuleBuilder Operand)
    unops =
      M.fromList
        [("-", fneg)]

-- Binary Operands
genOperand (BinOp oper a b) localVars = do
  opA <- genOperand a localVars
  opB <- genOperand b localVars
  case M.lookup oper binops of
    Just f -> f opA opB
    Nothing -> genOperand (S.Call (Name ("binary_" <> oper)) [a, b]) localVars
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

-- If
genOperand (If cond thenExpr elseExpr) localVars = mdo
  computedCond <- genOperand cond localVars
  condBr computedCond ifThen ifElse
  ifThen <- block `named` "if.then"
  computedThen <- genOperand thenExpr localVars
  br ifExit
  ifElse <- block `named` "if.else"
  computedElse <- genOperand elseExpr localVars
  br ifExit
  ifExit <- block `named` "if.exit"
  phi [(computedThen, ifThen), (computedElse, ifElse)]

-- Let in
-- genOperand (Let (Name varName) value body) localVars = do
--   var <- alloca ASTType.double Nothing 0
--   computedValue <- genOperand value localVars
--   store var 0 computedValue
--   genOperand body (var : localVars)
genOperand x _ = error $ "This shouldn't have matched here: " <> show x

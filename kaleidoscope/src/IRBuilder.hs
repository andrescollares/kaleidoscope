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
import qualified Data.Text as T

import Debug.Trace

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


-- we don't have a way to name variables within the llvm ir, they are named by numbers
-- so we need to keep track of the variables ourselves
-- we do this by keeping a list of local variables
-- with their respective "alias" a.k.a the name that the user gave them
-- variable context used in:
-- - let in

type LocalVar = (Maybe ShortByteString, Operand) -- alias, value

genLevel :: Expr -> [Operand] -> IRBuilderT ModuleBuilder ()
genLevel e localVars = genOperand e (localVarsFallback localVars) >>= ret

localVarsFallback :: [Operand] -> [LocalVar]
localVarsFallback = map (\operand -> (Nothing, operand))

-- Generates the Operands that codegenTop needs.
genOperand :: Expr -> [LocalVar] -> IRBuilderT ModuleBuilder Operand
-- Float
genOperand (Float n) _ = return $ ConstantOperand (C.Float (F.Double n))
-- Variables
genOperand (Var (Name n)) localVars = do
  -- if localVars has it then it's a local reference otherwise mark it as a global reference
  -- local variable names end in "_number" so we need to take that into consideration
  -- also local variable names can have "_"
  case getLocalVarName n localVars of
    Just (alias, localVar) -> trace ("\tvariable reference: " ++ show n) $ return localVar
    Nothing -> trace ("\tfailed: global: " ++ show n) $ load (ConstantOperand (C.GlobalReference (ASTType.ptr ASTType.double) (Name n))) 0
  where
    getLocalVarName :: ShortByteString -> [LocalVar] -> Maybe LocalVar
    getLocalVarName n vars = findLast (\localVar -> matchName localVar n) vars Nothing
    matchName :: LocalVar -> ShortByteString -> Bool
    matchName (Just varName, _) n = varName == n
    matchName (Nothing, LocalReference _ (Name varName)) n = removeEnding varName == n
    matchName (Nothing, LocalReference _ (UnName varNumber)) n = show varNumber == show n
    findLast :: (a -> Bool) -> [a] -> Maybe a -> Maybe a
    findLast p (x : xs) res
      | p x = findLast p xs (Just x)
      | otherwise = findLast p xs res
    findLast _ [] res = res
    -- TODO: Rework this function later, don't use show
    -- bytestring > 11.smth has implemented this function but llvm 12 doesn't permit bytestring > 11
    removeEnding :: ShortByteString -> ShortByteString
    removeEnding n
      | T.isInfixOf "_" (T.pack $ show n) = fromString $ tail $ reverse $ tail $ dropWhile (/= '_') (reverse $ show n)
      | otherwise = n

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
    Just f -> trace ("\tbinopA: " ++ show opA ++ "\n\tbinopB: " ++ show opB) $ f opA opB
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
genOperand (Let (Name varName) value body) localVars = do
  var <- alloca ASTType.double Nothing 0
  computedValue <- genOperand value localVars
  trace ("\tlet var: " ++ show var ++ " = " ++ show computedValue) $ store var 0 computedValue
  loadedVar <- load var 0
  -- TODO: alloca -> store -> load: there's probably a better way to do this
  genOperand body ((Just varName, loadedVar) : localVars)

genOperand x _ = error $ "This shouldn't have matched here: " <> show x

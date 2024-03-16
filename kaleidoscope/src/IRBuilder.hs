{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecursiveDo #-}

module IRBuilder where

-- import Data.Maybe

import Control.Monad.RWS (gets)
import Data.Bifunctor (first)
import Data.ByteString.Short
import qualified Data.Map.Strict as M
import Data.String
import qualified Data.Text as T
import JIT
import Types
import Instructions
import LLVM.AST as AST hiding (function)
import qualified LLVM.AST.Constant as C
import qualified LLVM.AST.Float as F
import LLVM.AST.FloatingPointPredicate (FloatingPointPredicate (UEQ, UGE, UGT, ULE, ULT, UNE))
import qualified LLVM.AST.IntegerPredicate as IP
import LLVM.AST.Global (Global (name, GlobalVariable, type'), parameters, returnType)
import qualified LLVM.AST.Global as G (Global (name, type'))
import LLVM.AST.Type (ptr)
import qualified LLVM.AST.Type as ASTType
import LLVM.IRBuilder.Instruction
import LLVM.IRBuilder.Internal.SnocList
import LLVM.IRBuilder.Module (ModuleBuilder, ModuleBuilderState (ModuleBuilderState, builderDefs, builderTypeDefs), MonadModuleBuilder (liftModuleState), execModuleBuilder, extern, function, global, ParameterName (ParameterName))
import LLVM.IRBuilder.Monad
import Syntax as S
import qualified Data.List as DL
import Debug.Trace

-- Generates the Module from the previous module and the new expressions
-- Has to optimize the module
-- Has to execute the module
-- Has to update the module state
genModule :: [Definition] -> [Expr] -> IO (Double, [Definition])
genModule oldDefs expressions = do
  optMod <- optimizeModule unoptimizedAst
  res <- runJIT optMod moduleMainFnType
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
    moduleMainFn = filter (\case
        GlobalDefinition AST.Function {name = Name "main"} -> True;
        _ -> False
      ) definitions
    moduleMainFnType = case moduleMainFn of
      [GlobalDefinition AST.Function {returnType = IntegerType {typeBits = 32}}] -> ASTType.i32
      _ -> ASTType.double

buildModuleWithDefinitions :: [Definition] -> ModuleBuilder a -> [Definition]
buildModuleWithDefinitions prevDefs = execModuleBuilder oldModl
  where
    oldModl = ModuleBuilderState {builderDefs = SnocList (reverse prevDefs), builderTypeDefs = mempty}

functionLocalVar :: [AST.Operand] -> [(S.Type, ParameterName)] -> Name -> AST.Type -> [LocalVar]
functionLocalVar operands functionParameters (Name n) t = localVarsFallback operands ++ [(Just n, getFunctionOperand (Name n) t (functionLocalVarParameters functionParameters, False))]
functionLocalVar _ _ _ _ = error "Function lacks a name."

functionLocalVarParameters :: [(S.Type, ParameterName)] -> [Parameter]
functionLocalVarParameters = map (\(t, ParameterName n) -> Parameter (getASTType t) (Name n) [])

localVarsFallback :: [AST.Operand] -> [LocalVar]
localVarsFallback = map (\operand -> (Nothing, operand))

-- Generates functions, constants, externs, definitions and a main function otherwise
-- The result is a ModuleBuilder monad
genTopLevel :: Expr -> ModuleBuilder AST.Operand
-- Extern definition
genTopLevel (S.TopLevel (S.Extern externName externArgs Integer)) = do
  extern externName (map (getASTType . fst) externArgs) ASTType.i32
genTopLevel (S.TopLevel (S.Extern externName externArgs Double)) = do
  extern externName (map (getASTType . fst) externArgs) ASTType.double
genTopLevel (S.TopLevel (S.Extern externName externArgs Boolean)) = do
  extern externName (map (getASTType . fst) externArgs) ASTType.i1
-- Function definition
genTopLevel (S.TopLevel (S.Function functionName functionArgs Double body)) = do
  function functionName (first getASTType <$> functionArgs) ASTType.double (\ops ->
    genLevel body $ functionLocalVar ops functionArgs functionName ASTType.double)
genTopLevel (S.TopLevel (S.Function functionName functionArgs Integer body)) = do
  function functionName (first getASTType <$> functionArgs) ASTType.i32 (\ops ->
    genLevel body $ functionLocalVar ops functionArgs functionName ASTType.i32)
genTopLevel (S.TopLevel (S.Function functionName functionArgs Boolean body)) = do
  function functionName (first getASTType <$> functionArgs) ASTType.i1 (\ops ->
    genLevel body $ functionLocalVar ops functionArgs functionName ASTType.i1)
-- Constant definition
genTopLevel (S.TopLevel (S.Constant Double constantName (Float val))) = do
  global constantName ASTType.double (C.Float (F.Double val))
genTopLevel (S.TopLevel (S.Constant Integer constantName (Int val))) = do
  global constantName ASTType.i32 (C.Int 32 val)
genTopLevel (S.TopLevel (S.Constant Boolean constantName (Bool val))) = do
  global constantName ASTType.i1 (C.Int 1 (if val then 1 else 0))
genTopLevel (S.TopLevel (S.Constant {})) = error "This shouldn't have matched here."
-- Unary operator definition
genTopLevel (S.Operand (S.UnaryDef unaryOpName unaryArgs body)) = do
  function (Name ("unary_" <> unaryOpName)) (map (\x -> (ASTType.double, x)) unaryArgs) ASTType.double (\_ -> genLevel body []) -- TODO: localVars
-- Binary operator definition
genTopLevel (S.Operand (S.BinaryDef binaryOpName binaryArgs body)) = do
  function (Name ("binary_" <> binaryOpName)) (map (\x -> (ASTType.double, x)) binaryArgs) ASTType.double (\_ -> genLevel body []) -- TODO: localVars
-- Any expression
genTopLevel (S.Operand expression) = do
  eType <- expressionType
  function "main" [] eType (\_ -> genLevel expression [])
    -- expressionType = getExpressionType expression
    where
      expressionType = case expression of
        -- if the expression is a call, we need to get the type of the function
        (S.Call fn _) -> do
          currentDefs <- liftModuleState $ gets builderDefs
          let maybeDef = getFunctionFromDefs currentDefs fn
          case maybeDef of
            Just def -> do
              case def of
                (GlobalDefinition AST.Function {returnType = retT}) -> return retT
                _ -> error $ "Function " <> show fn <> " not found."
            Nothing -> error $ "Function " <> show fn <> " not found."
        (S.Let varType varName _ expr) -> do
          return $ getExpressionType expr [(varName, varType)]
        -- TODO: carry over variables
        -- e.g. define a global and have the type in the list
        -- type can only be obtained within IRBuilderT ModuleBuilder
        _ -> return $ getExpressionType expression []

-- we don't have a way to name variables within the llvm ir, they are named by numbers
-- so we need to keep track of the variables ourselves
-- we do this by keeping a list of local variables
-- with their respective "alias" a.k.a the name that the user gave them
-- variable context used in:
-- - let in

-- varName, value

type LocalVar = (Maybe ShortByteString, AST.Operand) -- alias, value

getLocalVarName :: ShortByteString -> [LocalVar] -> Maybe LocalVar
getLocalVarName n = DL.find (`matchName` n)
matchName :: LocalVar -> ShortByteString -> Bool
matchName (Just varName, _) n = varName == n
matchName (Nothing, LocalReference _ (Name varName)) n = removeEnding varName == n
matchName (Nothing, LocalReference _ (UnName varNumber)) n = show varNumber == show n
matchName _ _ = False
-- TODO: Rework this function later, don't use show
-- bytestring > 11.smth has implemented this function but llvm 12 doesn't permit bytestring > 11
removeEnding :: ShortByteString -> ShortByteString
removeEnding variableName
  | T.isInfixOf "_" (T.pack $ show variableName) = fromString $ tail $ reverse $ tail $ dropWhile (/= '_') (reverse $ show variableName)
  | otherwise = variableName

genLevel :: S.Operand -> [LocalVar] -> IRBuilderT ModuleBuilder ()
genLevel e localVars = do
  generated <- genOperand e localVars
  ret generated

-- Generates the Operands that genTopLevel needs.
genOperand :: S.Operand -> [LocalVar] -> IRBuilderT ModuleBuilder AST.Operand
-- Float
genOperand (Float n) _ = return $ ConstantOperand (C.Float (F.Double n))
-- Integer
genOperand (Int n) _ = return $ ConstantOperand (C.Int 32 n)
-- Bool
genOperand (Bool b) _ = return $ ConstantOperand (C.Int 1 (if b then 1 else 0))
-- Variables
genOperand (Var (Name nameString)) localVars = do
  -- if localVars has it then it's a local reference otherwise mark it as a global reference
  -- local variable names end in "_number" so we need to take that into consideration
  -- also local variable names can have "_"
  case getLocalVarName nameString localVars of
    Just (_, localVar) -> return localVar
    Nothing -> do
      currentDefs <- liftModuleState $ gets builderDefs
      let maybeDef = trace ("Constants: " ++ show currentDefs) getConstantFromDefs currentDefs (Name nameString)
      case maybeDef of
        Just def -> do
          case def of
            (GlobalDefinition AST.GlobalVariable {G.type' = t}) -> load (ConstantOperand (C.GlobalReference (ASTType.ptr t) (Name nameString))) 0
            _ -> error $ "Constant " <> show nameString <> " not found."
        Nothing -> error $ "Constant " <> show nameString <> " not found."

      -- load (ConstantOperand (C.GlobalReference (ASTType.ptr ASTType.double) (Name nameString))) 0
    -- possible Exception: EncodeException "reference to undefined global: Name \"a\""

-- Call
genOperand (S.Call (Name fnName) functionArgs) localVars = do
  largs <- mapM (`genOperand` localVars) functionArgs
  let functionDefinition = getLocalVarName fnName localVars
  case functionDefinition of
    Just (_, localVar) -> call localVar (map (\x -> (x, [])) largs)
    Nothing -> do
      currentDefs <- liftModuleState $ gets builderDefs
      let maybeDef = getFunctionFromDefs currentDefs (Name fnName)
      case maybeDef of
        Just def -> do
          case def of
            (GlobalDefinition AST.Function {returnType = retT, parameters = params}) -> call (getFunctionOperand (Name fnName) retT params) (map (\x -> (x, [])) largs)
            _ -> error $ "Function " <> show fnName <> " not found."
        Nothing -> error $ "Function " <> show fnName <> " not found."

-- Unary Operands
genOperand (UnaryOp oper a) localVars = do
  op <- genOperand a localVars
  case M.lookup oper unops of
    Just f -> f op
    Nothing -> error "This shouldn't have matched here, unary operand doesn't exist."
  where
    unops :: M.Map ShortByteString (AST.Operand -> IRBuilderT ModuleBuilder AST.Operand)
    unops =
      M.fromList
        [("-", fneg)]

-- Binary Operands
genOperand (BinOp oper a b) localVars = do
  opA <- genOperand a localVars
  opB <- genOperand b localVars
  case M.lookup oper $ binops opA opB of
    Just f -> f opA opB
    Nothing -> genOperand (S.Call (Name ("binary_" <> oper)) [a, b]) localVars
  where
    binops :: AST.Operand -> AST.Operand -> M.Map ShortByteString (AST.Operand -> AST.Operand -> IRBuilderT ModuleBuilder AST.Operand)
    binops firstOp secondOp =
      M.fromList
        [ ("+", eitherType add fadd),
          ("-", eitherType sub fsub),
          ("*", eitherType mul fmul),
          ("/", eitherType udiv fdiv),
          ("<", eitherType (icmp IP.ULT) (fcmp ULT)),
          (">", eitherType (icmp IP.UGT) (fcmp UGT)),
          ("==", eitherType (icmp IP.EQ) (fcmp UEQ)),
          ("!=", eitherType (icmp IP.NE) (fcmp UNE)),
          ("<=", eitherType (icmp IP.ULE) (fcmp ULE)),
          (">=", eitherType (icmp IP.UGE) (fcmp UGE))
        ]
      where
        eitherType = typedOperandInstruction firstOp secondOp

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
genOperand (Let Double (Name varName) variableValue body) localVars = do
  var <- alloca ASTType.double Nothing 0
  computedValue <- genOperand variableValue localVars
  store var 0 computedValue
  loadedVar <- load var 0
  -- TODO: alloca -> store -> load: there's probably a better way to do this
  genOperand body ((Just varName, loadedVar) : localVars)


genOperand (Let Integer (Name varName) variableValue body) localVars = do
  var <- alloca ASTType.i32 Nothing 0
  computedValue <- genOperand variableValue localVars
  store var 0 computedValue
  loadedVar <- load var 0
  genOperand body ((Just varName, loadedVar) : localVars)
genOperand (Let Boolean (Name varName) variableValue body) localVars = do
  var <- alloca ASTType.i1 Nothing 0
  computedValue <- genOperand variableValue localVars
  store var 0 computedValue
  loadedVar <- load var 0
  genOperand body ((Just varName, loadedVar) : localVars)
genOperand x _ = error $ "This shouldn't have matched here: " <> show x

getFunctionFromDefs :: SnocList Definition -> Name -> Maybe Definition
getFunctionFromDefs defs functionName = find (\def -> matchNameGlobal def functionName) defs Nothing
  where
    matchNameGlobal :: Definition -> Name -> Bool
    matchNameGlobal (GlobalDefinition AST.Function {name = n}) nameToMatch = n == nameToMatch
    matchNameGlobal _ _ = False
    find :: (a -> Bool) -> SnocList a -> Maybe a -> Maybe a
    find p (SnocList (x : xs)) res
      | p x = Just x
      | otherwise = find p (SnocList xs) res
    find _ (SnocList []) res = res

getConstantFromDefs :: SnocList Definition -> Name -> Maybe Definition
getConstantFromDefs defs constantName = find (\def -> matchNameGlobal def constantName) defs Nothing
  where
    matchNameGlobal :: Definition -> Name -> Bool
    matchNameGlobal (GlobalDefinition AST.GlobalVariable {name = n}) nameToMatch = n == nameToMatch
    matchNameGlobal _ _ = False
    find :: (a -> Bool) -> SnocList a -> Maybe a -> Maybe a
    find p (SnocList (x : xs)) res
      | p x = Just x
      | otherwise = find p (SnocList xs) res
    find _ (SnocList []) res = res

getFunctionOperand :: Name -> AST.Type -> ([Parameter], Bool) -> AST.Operand
getFunctionOperand fn retType (params, _) = ConstantOperand $ C.GlobalReference (ptr $ FunctionType retType (map (\(AST.Parameter t _ _) -> t) params) False) fn

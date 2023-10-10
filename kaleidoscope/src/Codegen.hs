{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}

module Codegen where

import Control.Monad.State
import Data.ByteString.Short
import Data.Function as F
import qualified Data.List as L
import qualified Data.Map as Map
import Data.String
import Debug.Trace
import LLVM.AST
import qualified LLVM.AST as AST
import qualified LLVM.AST.Attribute as A
import qualified LLVM.AST.CallingConvention as CC
import qualified LLVM.AST.Constant as C
import qualified LLVM.AST.Float as F
import qualified LLVM.AST.FloatingPointPredicate as FP
import qualified LLVM.AST.Global as G
import qualified LLVM.AST.Linkage as L
import LLVM.AST.Type (ptr)

-------------------------------------------------------------------------------
-- Module Level
-------------------------------------------------------------------------------

newtype LLVM a = LLVM (State AST.Module a)
  deriving newtype (Functor, Applicative, Monad, MonadState AST.Module)

runLLVM :: AST.Module -> LLVM a -> AST.Module
runLLVM modl (LLVM mState) = execState mState modl

emptyModule :: ShortByteString -> AST.Module
emptyModule label = defaultModule {moduleName = label}

addDefn :: Definition -> LLVM ()
addDefn d = do
  defs <- gets moduleDefinitions
  -- TODO: find out if the definition already exists and replace
  modify $ \s -> s {moduleDefinitions = defs ++ [d]}

define :: Type -> ShortByteString -> [(Type, Name)] -> [BasicBlock] -> LLVM ()
define retty label argtys body =
  addDefn $
    GlobalDefinition $
      functionDefaults
        { G.name = Name label,
          G.parameters = ([Parameter ty nm [] | (ty, nm) <- argtys], False),
          G.returnType = retty,
          G.basicBlocks = body
        }

-- getOrAssignVar :: Type -> ShortByteString -> [BasicBlock] -> LLVM ()
-- getOrAssignVar var x = do
--   syms <- gets symtab
--   case lookup var syms of
--     Just x -> return x
--     Nothing -> do
--       -- globalVariable --"asdd"
--       x <- allocaGlobal double
--       assign var x
--       -- ConstantOperand (GlobalReference (FloatingPointType {floatingPointType = DoubleFP}) (UnName 1))
--       trace ("asdasd" ++ show x) $ return x

-- globalVariable :: Codegen Operand
-- globalVariable = do
--   n <- fresh
--   let ref = UnName n
--   instrGlobal $
--     Alloca
--       double
--       (Just (ConstantOperand
--             (C.GlobalReference (ptr double)
--             ref
--       )))
--       0
--       []
-- addDefn $
--   GlobalDefinition $
--     globalVariableDefaults
--     {
--         G.name = UnName 0,
--         G.type' = double
--         -- G.alignment = a,
--         -- G.section = s
--     }

external :: Type -> ShortByteString -> [(Type, Name)] -> LLVM ()
external retty label argtys =
  addDefn $
    GlobalDefinition $
      functionDefaults
        { G.name = Name label,
          G.linkage = L.External,
          G.parameters = ([Parameter ty nm [] | (ty, nm) <- argtys], False),
          G.returnType = retty,
          G.basicBlocks = []
        }

---------------------------------------------------------------------------------
-- Types
-------------------------------------------------------------------------------

-- IEEE 754 double
double :: Type
double = FloatingPointType DoubleFP

-------------------------------------------------------------------------------
-- Names
-------------------------------------------------------------------------------

type Names = Map.Map ShortByteString Int

uniqueName :: ShortByteString -> Names -> (ShortByteString, Names)
uniqueName nm ns =
  case Map.lookup nm ns of
    Nothing -> (nm, Map.insert nm 1 ns)
    Just ix -> (nm <> fromString (show ix), Map.insert nm (ix + 1) ns)

-------------------------------------------------------------------------------
-- Codegen State
-------------------------------------------------------------------------------

type SymbolTable = [(ShortByteString, Operand)]

data CodegenState = CodegenState
  { currentBlock :: Name, -- Name of the active block to append to
    blocks :: Map.Map Name BlockState, -- Blocks for function
    symtab :: SymbolTable, -- Function scope symbol table
    blockCount :: Int, -- Count of basic blocks
    count :: Word, -- Count of unnamed instructions
    names :: Names -- Name Supply
  }
  deriving stock (Show)

data BlockState = BlockState
  { idx :: Int, -- Block index
    stack :: [Named Instruction], -- Stack of instructions
    term :: Maybe (Named Terminator) -- Block terminator
  }
  deriving stock (Show)

-------------------------------------------------------------------------------
-- Codegen Operations
-------------------------------------------------------------------------------

newtype Codegen a = Codegen {runCodegen :: State CodegenState a}
  deriving newtype (Functor, Applicative, Monad, MonadState CodegenState)

sortBlocks :: [(Name, BlockState)] -> [(Name, BlockState)]
sortBlocks = L.sortBy (compare `on` (idx . snd))

createBlocks :: CodegenState -> [BasicBlock]
createBlocks m = map makeBlock $ sortBlocks $ Map.toList (blocks m)

makeBlock :: (Name, BlockState) -> BasicBlock
makeBlock (l, (BlockState _ s t)) = BasicBlock l (reverse s) (maketerm t)
  where
    maketerm (Just x) = x
    maketerm Nothing = error $ "Block has no terminator: " ++ (show l)

entryBlockName :: ShortByteString
entryBlockName = "entry"

emptyBlock :: Int -> BlockState
emptyBlock i = BlockState i [] Nothing

-- uno = cons $ C.Float (F.Double 1.0)

emptyCodegen :: CodegenState
emptyCodegen = CodegenState (Name entryBlockName) Map.empty [] 1 0 Map.empty

execCodegen :: Codegen a -> CodegenState
execCodegen m = execState (runCodegen m) emptyCodegen

fresh :: Codegen Word
fresh = do
  i <- gets count
  modify $ \s -> s {count = 1 + i}
  return $ i + 1

instr :: Instruction -> Codegen Operand
instr ins = do
  n <- fresh
  let ref = UnName n
  blk <- current
  let i = stack blk
  modifyBlock (blk {stack = (ref := ins) : i})
  return $ local ref

--
-- instrGlobal :: Instruction -> Codegen Operand
-- instrGlobal ins = do
--   n <- fresh
--   let ref = UnName n
--   blk <- current
--   let i = stack blk
--   modifyBlock (blk {stack = (ref := ins) : i})
--   return $ cons $ global ref

unnminstr :: Instruction -> Codegen ()
unnminstr ins = do
  blk <- current
  let i = stack blk
  modifyBlock (blk {stack = Do ins : i})

terminator :: Named Terminator -> Codegen (Named Terminator)
terminator trm = do
  blk <- current
  modifyBlock (blk {term = Just trm})
  return trm

-------------------------------------------------------------------------------
-- Block Stack
-------------------------------------------------------------------------------

entry :: Codegen Name
entry = gets currentBlock

addBlock :: ShortByteString -> Codegen Name
addBlock bname = do
  bls <- gets blocks
  ix <- gets blockCount
  nms <- gets names

  let new = emptyBlock ix
      (qname, supply) = uniqueName bname nms

  modify $ \s ->
    s
      { blocks = Map.insert (Name qname) new bls,
        blockCount = ix + 1,
        names = supply
      }
  return (Name qname)

setBlock :: Name -> Codegen Name
setBlock bname = do
  modify $ \s -> s {currentBlock = bname}
  return bname

getBlock :: Codegen Name
getBlock = gets currentBlock

modifyBlock :: BlockState -> Codegen ()
modifyBlock new = do
  active <- gets currentBlock
  modify $ \s -> s {blocks = Map.insert active new (blocks s)}

current :: Codegen BlockState
current = do
  c <- gets currentBlock
  blks <- gets blocks
  case Map.lookup c blks of
    Just x -> return x
    Nothing -> error $ "No such block: " ++ show c

-------------------------------------------------------------------------------
-- Symbol Table
-------------------------------------------------------------------------------

assign :: ShortByteString -> Operand -> Codegen ()
assign var x = do
  lcls <- gets symtab
  modify $ \s -> s {symtab = [(var, x)] ++ lcls}

newtype MaybeT m a = MaybeT {runMaybeT :: m (Maybe a)}

instance Monad m => Applicative (MaybeT m) where
  pure = return
  (<*>) = ap

instance Monad m => Functor (MaybeT m) where
  fmap = liftM

instance (Monad m) => Monad (MaybeT m) where
  return = MaybeT . return . Just
  x >>= f = MaybeT $ do
    v <- runMaybeT x
    case v of
      Nothing -> return Nothing
      Just y -> runMaybeT (f y)

getvar :: ShortByteString -> MaybeT Codegen Operand
getvar var = MaybeT $ do
  syms <- gets symtab
  case lookup var syms of
    -- LocalReference (FloatingPointType {floatingPointType = DoubleFP}) (UnName 1)
    Just x -> return $ Just x
    Nothing -> return Nothing -- error $ "Local variable not in scope: " ++ show var -- call (externf (AST.Name var) []) []
    -------------------------------------------------------------------------------

-- References
local :: Name -> Operand
local = LocalReference double

global :: Name -> C.Constant
global = C.GlobalReference double

externf :: Name -> [Operand] -> Operand
externf nm fargs =
  ConstantOperand
    ( C.GlobalReference
        ( ptr $
            FunctionType
              { resultType = double,
                argumentTypes = (map (\_ -> double) fargs),
                isVarArg = False
              }
        )
        nm
    )

-- Arithmetic and Constants
fadd :: Operand -> Operand -> Codegen Operand
fadd a b = instr $ FAdd noFastMathFlags a b []

fsub :: Operand -> Operand -> Codegen Operand
fsub a b = instr $ FSub noFastMathFlags a b []

fmul :: Operand -> Operand -> Codegen Operand
fmul a b = instr $ FMul noFastMathFlags a b []

fdiv :: Operand -> Operand -> Codegen Operand
fdiv a b = instr $ FDiv noFastMathFlags a b []

fcmp :: FP.FloatingPointPredicate -> Operand -> Operand -> Codegen Operand
fcmp cond a b = instr $ FCmp cond a b []

cons :: C.Constant -> Operand
cons = ConstantOperand

uitofp :: Type -> Operand -> Codegen Operand
uitofp ty a = instr $ UIToFP a ty []

toArgs :: [Operand] -> [(Operand, [A.ParameterAttribute])]
toArgs = map (\x -> (x, []))

-- Effects
call :: Operand -> [Operand] -> Codegen Operand
call fn fargs = instr $ Call Nothing CC.C [] (Right fn) (toArgs fargs) [] []

alloca :: Type -> Codegen Operand
alloca ty = instr $ Alloca ty Nothing 0 []

--
-- allocaGlobal :: Type -> Codegen Operand
-- allocaGlobal ty = instrGlobal $ Alloca ty Nothing 0 []

store :: Operand -> Operand -> Codegen ()
store pointer val = trace (show val) $ unnminstr $ Store False pointer val Nothing 0 []

load :: Operand -> Codegen Operand
load pointer = instr $ Load False pointer Nothing 0 []

-- Control Flow
br :: Name -> Codegen (Named Terminator)
br val = terminator $ Do $ Br val []

cbr :: Operand -> Name -> Name -> Codegen (Named Terminator)
cbr cond tr fl = terminator $ Do $ CondBr cond tr fl []

phi :: Type -> [(Operand, Name)] -> Codegen Operand
phi ty incoming = instr $ Phi ty incoming []

ret :: Operand -> Codegen (Named Terminator)
ret val = terminator $ Do $ Ret (Just val) []

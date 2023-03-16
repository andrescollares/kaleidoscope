{-# LANGUAGE OverloadedStrings #-}

module Example where

import LLVM.AST
import qualified LLVM.AST as AST
import LLVM.AST.Global
import LLVM.Context
import LLVM.Module
import qualified LLVM.AST.Constant as C
import qualified LLVM.AST.Float as F
import LLVM.AST.Type (ptr)

import Control.Monad.Except
import Data.ByteString.Char8 as BS

int :: Type
int = IntegerType 32

defAdd :: Definition
defAdd = GlobalDefinition functionDefaults
  { name = Name "add"
  , parameters =
      ( [ Parameter int (Name "a") []
        , Parameter int (Name "b") [] ]
      , False )
  , returnType = int
  , basicBlocks = [body]
  }
  where
    body = BasicBlock
        (Name "entry")
        [ Name "result" :=
            Add False  -- no signed wrap
                False  -- no unsigned wrap
                (LocalReference int (Name "a"))
                (LocalReference int (Name "b"))
                []]
        (Do $ Ret (Just (LocalReference int (Name "result"))) [])

defSub :: Definition
defSub = GlobalDefinition functionDefaults
  { name = Name "sub"
  , parameters =
      ( [ Parameter int (Name "a") []
        , Parameter int (Name "b") [] ]
      , False )
  , returnType = int
  , basicBlocks = [body]
  }
  where
    body = BasicBlock
        (Name "entry")
        [ Name "result" :=
            Sub False  -- no signed wrap
                False  -- no unsigned wrap
                (LocalReference int (Name "a"))
                (LocalReference int (Name "b"))
                []]
        (Do $ Ret (Just (LocalReference int (Name "result"))) [])

defId :: Definition
defId = GlobalDefinition functionDefaults
  { name = Name "id"
  , parameters =
      ( [ Parameter (ptr (FloatingPointType {floatingPointType = DoubleFP})) (Name "a") []]
      , False )
  , returnType = (FloatingPointType {floatingPointType = DoubleFP})
  , basicBlocks = [body]
  }
  where
    body = BasicBlock
        (Name "entry")
        [ Name "tmp_input_w0" := GetElementPtr {
                inBounds = True,
                address = LocalReference (ptr (FloatingPointType {floatingPointType = DoubleFP})) (Name "a"),
                indices = [ConstantOperand (C.Int 64 0)],
                AST.metadata = []
            },
        UnName 0 := Load {
                volatile = False,
                address = LocalReference (ptr (FloatingPointType {floatingPointType = DoubleFP})) (Name "tmp_input_w0"),
                maybeAtomicity = Nothing,
                AST.alignment = 8,
                AST.metadata = []
            }
        ]
        (Do $ Ret (Just (LocalReference (FloatingPointType {floatingPointType = DoubleFP}) (UnName 0))) [])


module_ :: AST.Module
module_ = defaultModule
  { moduleName = "basic"
  , moduleDefinitions = [defAdd, defSub, defId]
  }


toLLVM :: AST.Module -> IO ()
toLLVM mod = withContext $ \ctx -> do
  llvm <- withModuleFromAST ctx mod moduleLLVMAssembly
  BS.putStrLn llvm


main :: IO ()
main = toLLVM module_
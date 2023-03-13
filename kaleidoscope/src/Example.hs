{-# LANGUAGE OverloadedStrings #-}

module Example where

import LLVM.AST
import qualified LLVM.AST as AST
import LLVM.AST.Global
import LLVM.Context
import LLVM.Module

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
      ( [ Parameter int (Name "a") []]
      , False )
  , returnType = int
  , basicBlocks = [body]
  }
  where
    body = BasicBlock
        (Name "entry")
        [ Name "result" := Alloca {allocatedType = FloatingPointType {floatingPointType = DoubleFP}, numElements = Nothing, AST.alignment = 0, AST.metadata = []},
          Name "result1" := Store {volatile = False, address = LocalReference (FloatingPointType {floatingPointType = DoubleFP}) (UnName 1), value = LocalReference (FloatingPointType {floatingPointType = DoubleFP}) (Name "x"), maybeAtomicity = Nothing, AST.alignment = 0, AST.metadata = []}
        ]
        (Do $ Ret (Just (LocalReference int (Name "result"))) [])


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
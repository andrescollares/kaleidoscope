module Tuple where

import LLVM.AST
import LLVM.AST.Global hiding (alignment, metadata)
import LLVM.AST.Type
import qualified LLVM.AST.Constant as C
import Data.String
import Data.List ( intercalate )

import Types (structType)

tupleAccessor :: Integer -> [Type] -> Definition
tupleAccessor tupleIndex types =
      GlobalDefinition
      functionDefaults
        { name = Name (fromString $ "_accessor_" ++ intercalate "_" (map typeToString types) ++ "_" ++ show tupleIndex),
          parameters = ([Parameter (ptr $ structType types) (Name (fromString "targetStruct")) []], False),
          -- TODO: the param is a pointer, we should load it first
          -- or better yet, we should pass the struct by value, but it breaks GetElementPtr for some reason
          returnType = IntegerType 32,
          basicBlocks = [
                   BasicBlock (Name $ fromString "entry") [
                    Name (fromString "tmp_input_w0") := GetElementPtr {
                      inBounds = True,
                      address = LocalReference (structType types) (Name (fromString "targetStruct")),
                      indices = [ConstantOperand (C.Int 32 0), ConstantOperand (C.Int 32 tupleIndex)],
                      metadata = []
                    },
                    UnName 0 := Load {
                      volatile = False,
                      address = LocalReference (ptr (FloatingPointType DoubleFP)) (Name (fromString "tmp_input_w0")),
                      maybeAtomicity = Nothing,
                      alignment = 8,
                      metadata = []
                    }
                    ] (
                      Do $ Ret (Just (LocalReference (IntegerType 32) (UnName 0))) []
                    )
                  ]
        }

typeToString :: Type -> String
typeToString (IntegerType 32) = "i32"
typeToString (FloatingPointType DoubleFP) = "double"
typeToString (IntegerType 1) = "bool"
typeToString _ = error "Unsupported type"

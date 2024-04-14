{-# OPTIONS_GHC -Wno-incomplete-record-updates #-}

module StdLibrary where

import Data.String
import LLVM.AST
import LLVM.AST.Global hiding (alignment, metadata)
import qualified LLVM.AST.Constant as C
import LLVM.AST.Float
import LLVM.AST.Type (ptr)
import LLVM.AST.Constant (Constant(GlobalReference))
import Types (structType)
import Tuple (tupleAccessor)

stdLibrary :: [Definition]
stdLibrary =
  [ GlobalDefinition
      functionDefaults
        { name = Name (fromString "printi"),
          parameters = ([Parameter (IntegerType 32) (Name (fromString "i")) []], False),
          returnType = IntegerType 32,
          basicBlocks = []
        },
    GlobalDefinition
      functionDefaults
        { name = Name (fromString "printd"),
          parameters = ([Parameter (FloatingPointType DoubleFP) (Name (fromString "d")) []], False),
          returnType = FloatingPointType DoubleFP,
          basicBlocks = []
        },
    GlobalDefinition
      functionDefaults
        { name = Name (fromString "printb"),
          parameters = ([Parameter (IntegerType 1) (Name (fromString "b")) []], False),
          returnType = IntegerType 1,
          basicBlocks = []
        },
    GlobalDefinition
      globalVariableDefaults 
        { name = Name (fromString "cosa"),
          LLVM.AST.Global.type' = StructureType {
            isPacked = False,
            elementTypes = [IntegerType 32, FloatingPointType DoubleFP, IntegerType 1]
            },
          initializer = Just $ C.Struct {
            C.structName = Nothing,
            C.isPacked = False,
            C.memberValues = [
              C.Int 32 42,
              C.Float (Double 13.37),
              C.Int 1 1
              ]
            }
          },
    GlobalDefinition
      functionDefaults
        { name = Name (fromString "fst"),
          parameters = ([], False),
          returnType = IntegerType 32,
          basicBlocks = [
                   BasicBlock (Name $ fromString "foo") [ 
                    Name (fromString "tmp_input_w0") := GetElementPtr {
                      inBounds = True,
                      -- address = LocalReference (ptr StructureType {
                      --   isPacked = False,
                      --   elementTypes = [IntegerType 32, FloatingPointType DoubleFP, IntegerType 1]
                      -- }) (Name (fromString "cosa")),
                      address = ConstantOperand $
                        GlobalReference (ptr (StructureType {
                        isPacked = False,
                        elementTypes = [IntegerType 32, FloatingPointType DoubleFP, IntegerType 1]
                        })) (Name (fromString "cosa")),
                      -- Indices to retrieve the desired element, first index is always 0 for a simple struct
                      indices = [ConstantOperand (C.Int 32 0), ConstantOperand (C.Int 32 0)],
                      metadata = []
                    },
                    UnName 0 := Load {
                      volatile = False,
                      address = LocalReference (ptr (IntegerType 32)) (Name (fromString "tmp_input_w0")),
                      maybeAtomicity = Nothing,
                      alignment = 8,
                      metadata = []
                    }
                   ] (
                     Do $ Ret (Just (LocalReference (IntegerType 32) (UnName 0))) []
                   )
                  ]
              -- Ret (Just (
              --   ConstantOperand (C.Struct {
              --     C.structName = Nothing,
              --     C.isPacked = False,
              --     C.memberValues = [
              --       C.Int 32 42,
              --       C.Float (Double 42.0),
              --       C.Int 1 1
              --       ]
              --     })
              -- )) []
              
        },
    GlobalDefinition
      functionDefaults
        { name = Name (fromString "snd"),
          parameters = ([], False),
          returnType = FloatingPointType DoubleFP,
          basicBlocks = [
                   BasicBlock (Name $ fromString "foo") [ 
                    Name (fromString "tmp_input_w0") := GetElementPtr {
                      inBounds = True,
                      address = ConstantOperand $
                        GlobalReference (ptr (StructureType {
                        isPacked = False,
                        elementTypes = [IntegerType 32, FloatingPointType DoubleFP, IntegerType 1]
                        })) (Name (fromString "cosa")),
                      indices = [ConstantOperand (C.Int 32 0), ConstantOperand (C.Int 32 1)],
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
                     Do $ Ret (Just (LocalReference (FloatingPointType DoubleFP) (UnName 0))) []
                   )
                  ]
        },
      -- fst and snd tuple accessors
      -- these functions should be called by another higher level function
      -- fst & snd should call the appropiate function to get the first element of the tuple
      -- depending on the types of the tuple.
      tupleAccessor 0 [IntegerType 32, FloatingPointType DoubleFP, IntegerType 1],
      tupleAccessor 1 [IntegerType 32, FloatingPointType DoubleFP, IntegerType 1]
  ]
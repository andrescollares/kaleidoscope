{-# OPTIONS_GHC -Wno-incomplete-record-updates #-}

module StdLibrary where

import Data.String
import LLVM.AST
import LLVM.AST.Global
import qualified LLVM.AST.Constant as C
import LLVM.AST.Float

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
              C.Float (Double 42.0),
              C.Int 1 1
              ]
            }
          }
  ]
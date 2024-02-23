module StdLibrary where

import LLVM.AST
import LLVM.AST.Global
import Data.String

stdLibrary :: [Definition]
stdLibrary = [
    GlobalDefinition functionDefaults {
      name = Name (fromString "printi"),
      parameters = ([Parameter (IntegerType 32) (Name (fromString "i")) []], False),
      returnType = IntegerType 32,
      basicBlocks = []
    },
    GlobalDefinition functionDefaults {
      name = Name (fromString "printd"),
      parameters = ([Parameter (FloatingPointType DoubleFP) (Name (fromString "d")) []], False),
      returnType = VoidType,
      basicBlocks = []
    },
    GlobalDefinition functionDefaults {
      name = Name (fromString "printb"),
      parameters = ([Parameter (IntegerType 1) (Name (fromString "b")) []], False),
      returnType = VoidType,
      basicBlocks = []
    }
   ]
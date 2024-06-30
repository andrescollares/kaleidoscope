{-# OPTIONS_GHC -Wno-incomplete-record-updates #-}

module StdLibrary where

import Data.String
import LLVM.AST hiding (type')
import LLVM.AST.Global hiding (alignment, metadata, callingConvention, returnAttributes, functionAttributes)
import LLVM.AST.AddrSpace
import LLVM.AST.Linkage (Linkage(External))
import LLVM.AST.Visibility (Visibility(Default))
import qualified LLVM.AST.Constant as C
import qualified LLVM.AST.CallingConvention as CC

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
    TypeDefinition (Name (fromString "IntList")) $ Just (StructureType False [
      IntegerType 32,
      PointerType (NamedTypeReference (Name (fromString "IntList"))) (AddrSpace 0)
    ]),
    GlobalDefinition
      functionDefaults
       { name = Name (fromString "printil"),
         parameters = ([Parameter (PointerType (NamedTypeReference (Name (fromString "IntList"))) (AddrSpace 0)) (Name (fromString "list")) []], False),
         returnType = IntegerType 32,
         basicBlocks = []
       },
    GlobalDefinition
      globalVariableDefaults {
        name = Name (fromString "VoidIntList"),
        linkage = External,
        visibility = Default,
        isConstant = True,
        type' = PointerType (NamedTypeReference (Name (fromString "IntList"))) (AddrSpace 0),
        initializer = Just $ C.Null $ PointerType (NamedTypeReference (Name (fromString "IntList"))) (AddrSpace 0)
      },
    GlobalDefinition
      functionDefaults
        { name = Name (fromString "printIntMainFunction"),
          parameters = ([], False),
          returnType = IntegerType 32,
          basicBlocks = [
            BasicBlock (Name (fromString "entry")) [
              UnName 0 := Call {
                tailCallKind = Nothing,
                callingConvention = CC.C,
                returnAttributes = [],
                function = Right $ ConstantOperand $ C.GlobalReference (PointerType (FunctionType (IntegerType 32) [] False) (AddrSpace 0)) (Name (fromString "main")),
                arguments = [],
                functionAttributes = [],
                metadata = []
              },
              UnName 1 := Call {
                tailCallKind = Nothing,
                callingConvention = CC.C,
                returnAttributes = [],
                function = Right $ ConstantOperand $ C.GlobalReference (PointerType (FunctionType (IntegerType 32) [IntegerType {typeBits = 32}] False) (AddrSpace 0)) (Name (fromString "printi")),
                arguments = [ (LocalReference (IntegerType 32) (UnName 0), []) ],
                -- arguments = [ (ConstantOperand $ C.Int 32 42, []) ],
                functionAttributes = [],
                metadata = []
              }
            ] (
                  Do $ Ret (Just $ ConstantOperand $ C.Int 32 0) []
                 )
          ]
        }
  ]
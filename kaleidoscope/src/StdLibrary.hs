{-# OPTIONS_GHC -Wno-incomplete-record-updates #-}

module StdLibrary where

import Data.String
import LLVM.AST hiding (type')
import LLVM.AST.Global hiding (alignment, metadata, callingConvention, returnAttributes, functionAttributes)
import LLVM.AST.AddrSpace
import LLVM.AST.Linkage (Linkage(External))
import LLVM.AST.Visibility (Visibility(Default))
import qualified LLVM.AST.Constant as C
import CLI (CliOptions(..))
import Processor (process)

processLibrary :: String -> IO (Maybe [Definition])
processLibrary fname = do
  file <- readFile fname
  result <- process stdLibrary file (CliOptions {inputFile = "", failOnErrors = False, optimizationLevel = 3, emitLLVM = False})
  return $ snd <$> result

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
    TypeDefinition (Name (fromString "FloatList")) $ Just (StructureType False [
      FloatingPointType DoubleFP,
      PointerType (NamedTypeReference (Name (fromString "FloatList"))) (AddrSpace 0)
    ]),
    TypeDefinition (Name (fromString "BoolList")) $ Just (StructureType False [
      IntegerType 1,
      PointerType (NamedTypeReference (Name (fromString "BoolList"))) (AddrSpace 0)
    ]),
    GlobalDefinition
      functionDefaults
       { name = Name (fromString "printil"),
         parameters = ([Parameter (PointerType (NamedTypeReference (Name (fromString "IntList"))) (AddrSpace 0)) (Name (fromString "list")) []], False),
         returnType = IntegerType 32,
         basicBlocks = []
       },
    GlobalDefinition
      functionDefaults
       { name = Name (fromString "printfl"),
         parameters = ([Parameter (PointerType (NamedTypeReference (Name (fromString "FloatList"))) (AddrSpace 0)) (Name (fromString "list")) []], False),
         returnType = IntegerType 32,
         basicBlocks = []
       },
    GlobalDefinition
      functionDefaults
       { name = Name (fromString "printbl"),
         parameters = ([Parameter (PointerType (NamedTypeReference (Name (fromString "BoolList"))) (AddrSpace 0)) (Name (fromString "list")) []], False),
         returnType = IntegerType 32,
         basicBlocks = []
       },
    GlobalDefinition
      functionDefaults
       { name = Name (fromString "print_tuple"),
         parameters = ([Parameter (StructureType { elementTypes = [
            IntegerType 32,
            IntegerType 32
         ], isPacked = False }) (Name (fromString "tuple")) [],
         Parameter (IntegerType 32) (Name (fromString "first_type")) [],
         Parameter (IntegerType 32) (Name (fromString "second_type")) []
         ], False),
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
      }
      -- def length([int] l) -> int: if l == [] then 0 else 1 + length(tail(l));
      -- TODO: define length using the code above
      -- TODO: head and tail should accept different list types
      -- GlobalDefinition
      --   functionDefaults {
      --     name = Name (fromString "tail"),
      --     parameters = ([Parameter (PointerType (NamedTypeReference (Name (fromString "IntList"))) (AddrSpace 0)) (Name (fromString "list")) []], False),
      --     returnType = PointerType (NamedTypeReference (Name (fromString "IntList"))) (AddrSpace 0),
      --     basicBlocks = [
      --       BasicBlock (Name (fromString "entry")) [
      --         UnName 0 := Load {
      --           volatile = False,
      --           address = LocalReference (PointerType (NamedTypeReference (Name (fromString "IntList"))) (AddrSpace 0)) (Name (fromString "list")),
      --           maybeAtomicity = Nothing,
      --           alignment = 0,
      --           metadata = []
      --         },
      --         UnName 1 := ExtractValue {
      --           aggregate = LocalReference (NamedTypeReference (Name (fromString "IntList"))) (UnName 0),
      --           indices' = [1],
      --           metadata = []
      --         }
      --       ] (
      --         Do $ Ret (Just $ LocalReference (PointerType (NamedTypeReference (Name (fromString "IntList"))) (AddrSpace 0)) (UnName 1)) []
      --       )
      --     ]
      --   },
      -- GlobalDefinition
      --   functionDefaults {
      --     name = Name (fromString "head"),
      --     parameters = ([Parameter (PointerType (NamedTypeReference (Name (fromString "IntList"))) (AddrSpace 0)) (Name (fromString "list")) []], False),
      --     returnType = IntegerType 32,
      --     basicBlocks = [
      --       BasicBlock (Name (fromString "entry")) [
      --         UnName 0 := Load {
      --           volatile = False,
      --           address = LocalReference (PointerType (NamedTypeReference (Name (fromString "IntList"))) (AddrSpace 0)) (Name (fromString "list")),
      --           maybeAtomicity = Nothing,
      --           alignment = 0,
      --           metadata = []
      --         },
      --         UnName 1 := ExtractValue {
      --           aggregate = LocalReference (NamedTypeReference (Name (fromString "IntList"))) (UnName 0),
      --           indices' = [0],
      --           metadata = []
      --         }
      --       ] (
      --         Do $ Ret (Just $ LocalReference (IntegerType 32) (UnName 1)) []
      --       )
      --     ]
      --   }
  ]
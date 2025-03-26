{-# OPTIONS_GHC -Wno-incomplete-record-updates #-}
{-# LANGUAGE OverloadedStrings #-}

module StdLib.BaseDefs where

import Data.String (fromString)
import LLVM.AST (Definition (..),
 FloatingPointType (..), Name (..), Parameter (..),
 Type (..), Operand(..), Named(Do), Terminator( Ret ), returnOperand, operand0, Named( (:=) ), mkName)
import LLVM.AST.AddrSpace (AddrSpace (..))
import qualified LLVM.AST.Instruction as I ( Instruction( SIToFP, FPToSI, metadata, type' ), Terminator (metadata') )
import LLVM.AST.Global (Global (..), functionDefaults, BasicBlock (..))
import LLVM.AST.Type (i32, ptr, i8)
import LLVM.AST.Linkage (Linkage(External))
import LLVM.AST.Attribute (FunctionAttribute(OptimizeNone), ParameterAttribute (NonNull))


baseDefinitions :: [Definition]
baseDefinitions =
  [ 
    GlobalDefinition 
      functionDefaults
        { name        = mkName "printf"
        , linkage     = External
        , parameters  = ([Parameter ty (mkName "") [NonNull] | ty <- [ptr i8]], True)
        , returnType  = i32
        , functionAttributes = [Right OptimizeNone]
        },
    GlobalDefinition
       functionDefaults
         { name = Name (fromString "printb"),
           parameters = ([Parameter (IntegerType 1) (Name (fromString "b")) []], False),
           returnType = IntegerType 1,
           basicBlocks = []
         },
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
    -- math functions
    GlobalDefinition
      functionDefaults
        { name = Name (fromString "sin"),
          parameters = ([Parameter (FloatingPointType DoubleFP) (Name (fromString "x")) []], False),
          returnType = FloatingPointType DoubleFP,
          basicBlocks = []
        },
    GlobalDefinition
      functionDefaults
        { name = Name (fromString "cos"),
          parameters = ([Parameter (FloatingPointType DoubleFP) (Name (fromString "x")) []], False),
          returnType = FloatingPointType DoubleFP,
          basicBlocks = []
        },
    GlobalDefinition
      functionDefaults
        { name = Name (fromString "tan"),
          parameters = ([Parameter (FloatingPointType DoubleFP) (Name (fromString "x")) []], False),
          returnType = FloatingPointType DoubleFP,
          basicBlocks = []
        },
    GlobalDefinition
      functionDefaults
        { name = Name (fromString "log"),
          parameters = ([Parameter (FloatingPointType DoubleFP) (Name (fromString "x")) []], False),
          returnType = FloatingPointType DoubleFP,
          basicBlocks = []
        },
    GlobalDefinition
      functionDefaults
        { name = Name (fromString "fabs"),
          parameters = ([Parameter (FloatingPointType DoubleFP) (Name (fromString "x")) []], False),
          returnType = FloatingPointType DoubleFP,
          basicBlocks = []
        },
    GlobalDefinition
      functionDefaults
        { name = Name (fromString "rand"),
          parameters = ([], False),
          returnType = IntegerType 32,
          basicBlocks = []
        },
    GlobalDefinition
      functionDefaults
        { name = Name (fromString "srand"),
          parameters = ([Parameter (IntegerType 32) (Name (fromString "seed")) []], False),
          returnType = IntegerType 32,
          basicBlocks = []
        },
    -- misc functions
    GlobalDefinition
      functionDefaults
        { name = Name (fromString "exit"),
          parameters = ([Parameter (IntegerType 32) (Name (fromString "status")) []], False),
          returnType = IntegerType 32,
          basicBlocks = []
        },
    -- list building functions
    GlobalDefinition
      functionDefaults
        { name = Name (fromString "_alloc_int_list_node"),
          parameters = ([], False),
          returnType = PointerType (NamedTypeReference (Name (fromString "IntList"))) (AddrSpace 0),
          basicBlocks = []
        },
    GlobalDefinition
      functionDefaults
        { name = Name (fromString "_alloc_double_list_node"),
          parameters = ([], False),
          returnType = PointerType (NamedTypeReference (Name (fromString "FloatList"))) (AddrSpace 0),
          basicBlocks = []
        },
    GlobalDefinition
      functionDefaults
        { name = Name (fromString "_alloc_bool_list_node"),
          parameters = ([], False),
          returnType = PointerType (NamedTypeReference (Name (fromString "BoolList"))) (AddrSpace 0),
          basicBlocks = []
        },
    -- type definitions
    TypeDefinition (Name (fromString "IntList")) $
      Just
        ( StructureType
            False
            [ IntegerType 32,
              PointerType (NamedTypeReference (Name (fromString "IntList"))) (AddrSpace 0)
            ]
        ),
    TypeDefinition (Name (fromString "FloatList")) $
      Just
        ( StructureType
            False
            [ FloatingPointType DoubleFP,
              PointerType (NamedTypeReference (Name (fromString "FloatList"))) (AddrSpace 0)
            ]
        ),
    TypeDefinition (Name (fromString "BoolList")) $
      Just
        ( StructureType
            False
            [ IntegerType 1,
              PointerType (NamedTypeReference (Name (fromString "BoolList"))) (AddrSpace 0)
            ]
        ),
    -- GlobalDefinition
    --   globalVariableDefaults
    --     { name = Name (fromString "VoidIntList"),
    --       linkage = External,
    --       visibility = Default,
    --       isConstant = True,
    --       type' = PointerType (NamedTypeReference (Name (fromString "IntList"))) (AddrSpace 0),
    --       initializer = Just $ C.Null $ PointerType (NamedTypeReference (Name (fromString "IntList"))) (AddrSpace 0)
    --     },

    -- type casting functions
    GlobalDefinition
      functionDefaults
        { name = Name (fromString "int_to_double"),
          parameters = ([Parameter (IntegerType {typeBits = 32}) (Name "x_0") []],False),
          returnType = FloatingPointType DoubleFP,
          basicBlocks = [
            BasicBlock (UnName 0) [
              UnName 1 := I.SIToFP {operand0 = LocalReference (IntegerType {typeBits = 32}) (Name "x_0")
              , I.type' = FloatingPointType {floatingPointType = DoubleFP}, I.metadata = []
              }
              ] 
              (Do (Ret {returnOperand = Just (LocalReference (FloatingPointType {floatingPointType = DoubleFP}) (UnName 1)), I.metadata' = []}))]
        },
    GlobalDefinition
      functionDefaults
        { name = Name (fromString "double_to_int"),
          parameters = ([Parameter (FloatingPointType {floatingPointType = DoubleFP}) (Name "x_0") []],False),
          returnType = IntegerType {typeBits = 32},
          basicBlocks = [
            BasicBlock (UnName 0) [
              UnName 1 := I.FPToSI {operand0 = LocalReference (FloatingPointType {floatingPointType = DoubleFP}) (Name "x_0"), I.type' = IntegerType {typeBits = 32}, I.metadata = []
              }
              ]
              (Do (Ret { returnOperand = Just (LocalReference (IntegerType {typeBits = 32}) (UnName 1)), I.metadata' = []}))]
        }
  ]
genOptimizedMainModuleIR :: IO Module
genOptimizedMainModuleIR = genModule [Float 5.0]

genSimpleFunction :: IO Module
genSimpleFunction = genModule [S.Function "plus" ["x", "y"] (Float 5.0)]

genNotSoSimpleFunction :: IO Module
genNotSoSimpleFunction = genModule [S.Function "id" ["x"] (Var "x")]

genFunctionCall :: IO Module
genFunctionCall = genModule [ 
  S.Function "one" [] (Float 1.0), 
  S.Call "one" []
  ]

genIfFalse :: IO Module
genIfFalse = genModule [S.If (BinOp ">" (Float 0.0) (Float 1.0)) (Float 8.0) (Float 2.0)]

genIfTrue :: IO Module
genIfTrue = genModule [S.If (Float 1.0) (Float 5.0) (Float 2.0)]

genRecursive :: IO Module
genRecursive = genModule [
  S.Function "rec" ["x", "y"] (If (BinOp "<" (Var "x") (Float 1.0)) (Var "y") (S.Call "rec" [BinOp "-" (Var "x") (Float 1.0), BinOp "+" (Var "y") (Var "x")])),
  S.Call "rec" [Float 5.0, Float 0.0]
  ]


simple :: Module
simple = buildModule "exampleModule" $ do
  function "plus" [(ASTType.i32, "x"), (ASTType.i32, "y")] ASTType.i32 $ \[x, y] -> do
    r <- add x y
    ret r

conditional :: Module
conditional = buildModule "conditionModule" $ do
  -- This breaks the SSA Principle: https://stackoverflow.com/a/70901888
  -- f <- function "f" [(AST.i32, "a")] AST.i32 $ \[a] -> mdo
  --   cond <- icmp P.EQ a (ConstantOperand (C.Int 32 0))
  --   condBr cond ifThen ifElse
  --   ifThen <- block `named` "if.then"
  --   trVal <- add a (ConstantOperand (C.Int 32 0))
  --   ret trVal
  --   -- br ifExit
  --   ifElse <- block `named` "if.else"
  --   flVal <- add a (ConstantOperand (C.Int 32 0))
  --   ret flVal

  f <- function "f" [(ASTType.i32, "a")] ASTType.i32 $ \[a] -> mdo
    cond <- icmp P.EQ a (ConstantOperand (C.Int 32 0))
    condBr cond ifThen ifElse
    ifThen <- block `named` "if.then"
    trVal <- add a (ConstantOperand (C.Int 32 0))
    br ifExit
    ifElse <- block `named` "if.else"
    flVal <- add a (ConstantOperand (C.Int 32 0))
    br ifExit
    ifExit <- block `named` "if.exit"
    -- SSA
    r <- phi [(trVal, ifThen), (flVal, ifElse)]
    ret r

  function "main" [] ASTType.i32 $ \[] -> mdo
    -- the empty array are the parameter attributes: https://hackage.haskell.org/package/llvm-hs-pure-9.0.0/docs/LLVM-ASTType.ParameterAttribute.html
    r <- call f [(ConstantOperand (C.Int 32 0), [])]
    ret r

arithmetrics :: Module
arithmetrics = buildModule "arithmetrics" $ do
  -- function "+" [(ASTType.i32, "x"), (ASTType.i32, "y")] ASTType.i32 $ \[x, y] -> do
  --   r <- add x y
  --   ret r

  -- function "-" [(ASTType.i32, "x"), (ASTType.i32, "y")] ASTType.i32 $ \[x, y] -> do
  --   r <- sub x y
  --   ret r

  -- function "*" [(ASTType.i32, "x"), (ASTType.i32, "y")] ASTType.i32 $ \[x, y] -> do
  --   r <- mul x y
  --   ret r

  -- function "/" [(ASTType.i32, "x"), (ASTType.i32, "y")] ASTType.i32 $ \[x, y] -> do
  --   r <- sdiv x y
  --   ret r

  function "%" [(ASTType.i32, "x"), (ASTType.i32, "y")] ASTType.i32 $ \[x, y] -> do
    _ <- trace (show x) $ pure ()

    var <- fresh `named` "x"
    r <- srem (LocalReference ASTType.i32 var) y
    ret r

globalDef :: Module
globalDef = buildModule "variable_test" $ do
  x <- global "x" ASTType.double (C.Float (F.Double 50.0))

  function "main" [] ASTType.double $ \[] -> do
    x1 <- load (ConstantOperand (GlobalReference (ASTType.ptr ASTType.double) "y")) 0
    r <- fadd x1 (ConstantOperand (C.Float (F.Double 1.0)))
    -- res <- call (ConstantOperand (C.GlobalReference (ptr (FunctionType ASTType.i32 [ ] False)) (Name "f")) ) [ ]
    ret x1
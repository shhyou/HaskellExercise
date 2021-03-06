{-# LANGUAGE LambdaCase #-}

data Expr = Int Int
          | Var Int
          | Add1 Expr
          | Lam Expr
          | Ap Expr Expr
          deriving (Show)

omega = Ap (Lam (Ap (Var 0) (Var 0))) (Lam (Ap (Var 0) (Var 0)))
prog = (Ap (Lam (Int 0)) omega)

{- Normal Call-by-Name Interpreter -}

data Value0 = Clos0 ((() -> Value0) -> Value0)
            | N0 Int

instance Show Value0 where
  show (N0 n) = "(N0 " ++ show n ++ ")"
  show (Clos0 _) = "(closure)"

type Env0 = [() -> Value0]

interpL0 :: Expr -> Env0 -> Value0
interpL0 (Int n)    cxt = N0 n
interpL0 (Var m)    cxt = (cxt!!m) ()
interpL0 (Add1 e)   cxt =
  case interpL0 e cxt of
    N0 i -> N0 (i+1)
    _ -> error "interpL0: type mismatch"
interpL0 (Lam e)    cxt = Clos0 (\v -> interpL0 e (v:cxt))
interpL0 (Ap e1 e2) cxt =
  case interpL0 e1 cxt of 
    Clos0 f -> f (\() -> interpL0 e2 cxt)
    _ -> error $ "interpL0: type mismatch"

run0 e = interpL0 e []

{- CPS Call-by-Need Interpreter -}
data Value1 = Clos1 Env1 Expr
            | N1 Int

instance Show Value1 where
  show (N1 n) = "(N1 " ++ show n ++ ")"
  show (Clos1 _ _) = "(closure)"

type Env1 = [() -> (Value1 -> Value1) -> Value1]

interpL1 :: Expr -> Env1 -> (Value1 -> Value1) -> Value1
interpL1 (Int n)    cxt k = k (N1 n)
interpL1 (Var m)    cxt k = (cxt!!m) () k
interpL1 (Add1 e)   cxt k =
  interpL1 e cxt $ \case
    N1 i -> k (N1 (i+1))
    _ -> error "interpL1: type mismatch"
interpL1 (Lam e)    cxt k = k (Clos1 cxt e)
interpL1 (Ap e1 e2) cxt k =
  interpL1 e1 cxt $ \case 
    Clos1 cxt' e' -> interpL1 e' ((\() k -> interpL1 e2 cxt k):cxt') k
    _ -> error $ "interpL1: type mismatch"

{- Defunctionalized CPS Call-by-Need Interpreter -}
data Value2 = N2 Int
            | Clos2 Env2 Expr

instance Show Value2 where
  show (N2 n) = "(N2 " ++ show n ++ ")"
  show (Clos2 _ _) = "(closure)"

data Cont2 = Id2
           | Apply2 Env2 Expr Cont2
           | Add12 Cont2

data Thunk2 = Thunk2 Env2 Expr

type Env2 = [Thunk2]

applyL2 :: Cont2 -> (Value2 -> Value2)
applyL2 Id2               val             = val
applyL2 (Add12 k)         (N2 i)          = N2 (i+1)
applyL2 (Add12 _)         _               = error $ "applyL2: type mismatch"
applyL2 (Apply2 cxt e2 k) (Clos2 cxt' e') = interpL2 e' (Thunk2 cxt e2:cxt') k
applyL2 (Apply2 _ _ _)    _               = error $ "applyL2: type mismatch"

applyThunk2 :: Thunk2 -> Cont2 -> Value2
applyThunk2 (Thunk2 cxt e2) k = interpL2 e2 cxt k

interpL2 :: Expr -> Env2 -> Cont2 -> Value2
interpL2 (Int n)    cxt k = applyL2 k (N2 n)
interpL2 (Add1 e)   cxt k = interpL2 e cxt (Add12 k)
interpL2 (Lam e)    cxt k = applyL2 k (Clos2 cxt e)
interpL2 (Var m)    cxt k = applyThunk2 (cxt!!m) k
interpL2 (Ap e1 e2) cxt k = interpL2 e1 cxt (Apply2 cxt e2 k)

{- Defunctionalized CPS Call-by-Need Interpreter -}
data Value3 = N3 Int
            | Clos3 Env3 Expr

instance Show Value3 where
  show (N3 n) = "(N3 " ++ show n ++ ")"
  show (Clos3 _ _) = "(closure)"

data Cont3 = Id3
           | Apply3 Env3 Expr Cont3
           | Add13 Cont3
           | Imm3 Int Cont3

data Thunk3 = Thunk3 Env3 Expr

type Env3 = [Thunk3]

applyL3 :: Cont3 -> (Value3 -> Value3)
applyL3 Id3               val             = val
applyL3 (Add13 k)         (N3 i)          = N3 (i+1)
applyL3 (Add13 _)         _               = error $ "applyL3: type mismatch"
applyL3 (Apply3 cxt e2 k) (Clos3 cxt' e') = interpL3 e' (Thunk3 cxt e2:cxt') k
applyL3 (Apply3 _ _ _)    _               = error $ "applyL3: type mismatch"
applyL3 (Imm3 n k)        _               = applyL3 k (N3 n)

applyThunk3 :: Thunk3 -> Cont3 -> Value3
applyThunk3 (Thunk3 cxt e2) k = interpL3 e2 cxt k

interpL3 :: Expr -> Env3 -> Cont3 -> Value3
interpL3 (Int n)    cxt k = applyL3 (Imm3 n k) undefined
interpL3 (Add1 e)   cxt k = interpL3 e cxt (Add13 k)
interpL3 (Lam e)    cxt k = applyL3 k (Clos3 cxt e)
interpL3 (Var m)    cxt k = applyThunk3 (cxt!!m) k
interpL3 (Ap e1 e2) cxt k = interpL3 e1 cxt (Apply3 cxt e2 k)

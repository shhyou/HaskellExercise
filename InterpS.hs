{-# LANGUAGE LambdaCase #-}

import Debug.Trace (trace)

data Expr = Int Int
          | Var Int
          | Lam Expr
          | Ap Expr Expr
          deriving (Show)

omega = Ap (Lam (Ap (Var 0) (Var 0))) (Lam (Ap (Var 0) (Var 0)))
prog = (Ap (Lam (Int 0)) omega)

{- Normal Strict Interpreter -}

data Value0 = Clos0 (Value0 -> Value0)
            | N0 Int

type Env0 = [Value0]

instance Show Value0 where
  show (N0 n) = "(N0 " ++ show n ++ ")"
  show (Clos0 _) = "(closure)"

interpS0 :: Expr -> Env0 -> Value0
interpS0 (Int n)    cxt = N0 n
interpS0 (Var m)    cxt = cxt!!m
interpS0 (Lam e)    cxt = Clos0 (\v -> interpS0 e (v:cxt))
interpS0 (Ap e1 e2) cxt =
  case interpS0 e1 cxt of 
    Clos0 f ->
      case interpS0 e2 cxt of
        v@(N0 _)    -> f v
        v@(Clos0 _) -> f v
    _ -> error $ "interpS0: type mismatch"

run0 e = interpS0 e []

{- Strict CPS Interpreter -}
data Value1 = Clos1 Env1 Expr
            | N1 Int

instance Show Value1 where
  show (N1 n) = "(N1 " ++ show n ++ ")"
  show (Clos1 _ _) = "(closure)"

type Env1 = [Value1]

interpS1 :: Expr -> Env1 -> (Value1 -> Value1) -> Value1
interpS1 (Int n)    cxt k = k (N1 n)
interpS1 (Var m)    cxt k = k (cxt!!m)
interpS1 (Lam e)    cxt k = k (Clos1 cxt e)
interpS1 (Ap e0 e1) cxt k =
  interpS1 e0 cxt $ \case
    Clos1 cxt' e' ->
      interpS1 e1 cxt $ \case
        v -> interpS1 e' (v:cxt') k
    _ -> error $ "interpS1: type mismatch"


{- Strict CPS Defunctionalized Interpreter -}
data Value2 = Clos2 Env2 Expr
            | N2 Int

type Env2 = [Value2]

data Cont2 = Id2
           | Arg2 Env2 Expr Cont2
           | Apply2 Env2 Expr Cont2

applyS2 :: Cont2 -> (Value2 -> Value2)
applyS2 Id2                val             = val
applyS2 (Arg2 cxt e1 k)    (Clos2 cxt' e') = interpS2 e1 cxt (Apply2 cxt' e' k)
applyS2 (Arg2 _ _ _)       _               = error $ "applyS2: type mismatch"
applyS2 (Apply2 cxt' e' k) val             = interpS2 e' (val:cxt') k

interpS2 :: Expr -> Env2 -> Cont2 -> Value2
interpS2 (Int n)    cxt k = applyS2 k (N2 n)
interpS2 (Var m)    cxt k = applyS2 k (cxt!!m)
interpS2 (Lam e)    cxt k = applyS2 k (Clos2 cxt e)
interpS2 (Ap e0 e1) cxt k = interpS2 e0 cxt (Arg2 cxt e1 k)

{-# LANGUAGE FlexibleContexts #-}

module Pi where

import Control.Monad
import Control.Monad.Except

type Err = String

data Term = Var Int                 -- infer
          | Ap Term Term

          | Lam String Term         -- check

          | Pi String Term Term     -- types

          | U

          deriving (Show, Eq)

type Context = [(String, Term)]

contextLookup :: Context -> Int -> Term
contextLookup = (snd .) . (!!)

contextExtend :: String -> Term -> Context -> Context
contextExtend = ((:) .) . (,)

apply :: Term -> Term -> Term
apply e v = subst 0 e v where
  subst :: Int -> Term -> Term -> Term
  subst varNo (Var m)    v = if varNo == m then shift varNo 0 v else Var m
  subst varNo (Ap e1 e2) v = Ap (subst varNo e1 v) (subst varNo e2 v)
  subst varNo (Lam x e)  v = Lam x (subst (varNo+1) e v)
  subst varNo (Pi x t e) v = Pi x (subst varNo t v) (subst (varNo+1) e v)
  subst varNo U          v = U

  shift :: Int -> Int -> Term -> Term
  shift shiftDepth boundNum (Var m)    = if m > boundNum then Var (m+shiftDepth) else Var m
  shift shiftDepth boundNum (Ap e1 e2) = Ap (shift shiftDepth boundNum e1) (shift shiftDepth boundNum e2)
  shift shiftDepth boundNum (Lam x e)  = Lam x (shift shiftDepth (boundNum+1) e)
  shift shiftDepth boundNum (Pi x t e) = Pi x (shift shiftDepth boundNum t) (shift shiftDepth (boundNum+1) e)
  shift shiftDepth boundNum U          = U

checkError cxt e t = throwError ("Check (" ++ show e ++ " : " ++ show t ++ ") failed in context " ++ show cxt)

{-
check [] (Lam (Lam (Var 0))) (Pi U (Pi (Var 0) (Var 1)))
  check [] U U
  check [U] (Lam (Var 0)) (Pi (Var 0) (Var 1))
    check [U] (Var 0) U
    check [Var 1, U] (Var 0)
-}

check :: (MonadError Err m) => Context -> Term -> Term -> m ()
check cxt e'@(Lam x e) t'@(Pi x' a b) = do
  when (x /= x') (throwError $ "check failed: " ++ x ++ " /= " ++ x' ++
                              " in (" ++ show e' ++ " : " ++ show t' ++ ")")
  check cxt a U
  check (contextExtend x a cxt) e b
check cxt e@(Lam _ _) t = checkError cxt e t
check cxt (Pi x a b) U = do
  check cxt a U
  check (contextExtend x a cxt) b U
check cxt e@(Pi _ _ _) t = checkError cxt e t
check cxt U U = return ()
check cxt e@U t = checkError cxt e t
check cxt e t = do
  t' <- infer cxt e
  -- "check cxt e t: TODO: compare t and t'"
  when (t /= t') (checkError cxt e t)
  return ()

infer :: (MonadError Err m) => Context -> Term -> m Term
infer cxt (Var n) = return (contextLookup cxt n)
infer cxt (Ap e1 e2) = do
  t1 <- infer cxt e1
  case t1 of
    Pi x a b -> do
      check cxt e2 a
      return (apply b e2)
infer cxt e = throwError ("Cannot infer (" ++ show e ++ ") in " ++ show cxt)

-- (\A. \x. x) : All[A : U] All[x : A] A
e1 = Lam "" (Lam "" (Var 0))
t1 = Pi ""  U (Pi "" (Var 0) (Var 1))

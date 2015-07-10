{-# LANGUAGE FlexibleContexts #-}

module Pi where

import Control.Monad
import Control.Monad.Except

type Err = String

data Term = Var Int
          | Ap Term Term

          | Lam String Term         -- check

          | Pi String Term Term     -- types

          | U

          deriving (Show, Eq)

type Context = [(String, Term)]

contextLookup :: Context -> Int -> m Term
contextLookup = (snd .) . (!!)

contextExtend :: String -> Term -> Context -> Context
contextExtend = ((:) .) . (,)

data Value = Fun (Value -> Value)
           | Term Term

apply :: (MonadError Err m) => Context -> Term -> Term -> m Term
apply cxt e x v = return . concretize =<< abstract (contextExtend x v cxt) e where
  abstract :: Context -> Term -> Value
  abstract cxt (Var n) = abstract cxt (contextLookup cxt n)
  abstract cxt e@(Ap e1 e2) =
    case abstract cxt e1 of
      Fun f -> f (abstract cxt e2)
      _ -> error ("Try to apply non-function in expression " ++ show e ++ " in " ++ show cxt)
  abstract cxt (Lam x e2) = Fun (\v -> abstract (contextExtend x v cxt) e2)

  concretize :: Value -> Term
  concretize = undefined

checkError e t = throwError ("Check (" ++ show e ++ " : " ++ show t ++ ") failed")

check :: (MonadError Err m) => Context -> Term -> Term -> m ()
check cxt e'@(Lam x e) t'@(Pi x' a b) = do
  when (x /= x') (throwError $ "check failed: " ++ x ++ " /= " ++ x' ++
                              " in (" ++ show e' ++ " : " ++ show t' ++ ")")
  check cxt a U
  check (contextExtend x a cxt) e b
check cxt e@(Lam _ _) t = checkError e t
check cxt (Pi x a b) U = do
  check cxt a U
  check (contextExtend x a cxt) b U
check cxt e@(Pi _ _ _) t = checkError e t
check cxt U U = return ()
check cxt e@U t = checkError e t
check cxt e t = do
  t' <- infer cxt e
  error "check cxt e t: TODO: compare t and t'"
  return ()

infer :: (MonadError Err m) => Context -> Term -> m Term
infer cxt (Var n) = return (contextLookup cxt n)
infer cxt (Ap e1 e2) = do
  t1 <- infer cxt e1
  case t1 of
    Pi x a b -> do
      check cxt e2 a
      return (apply cxt b e2)
infer cxt e = throwError ("Cannot infer (" ++ show e ++ ") in " ++ show cxt)

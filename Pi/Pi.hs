{-# LANGUAGE FlexibleContexts #-}

module Pi where

import Control.Applicative ((<$>), Applicative(..))
import Control.Monad
import Control.Monad.State
import Control.Monad.Except

type Err = String
type Name = String

append :: Name -> Name -> Name
append = (++)

data Term = Var Name              -- infer
          | Ap Term Term

          | Lam Name Term         -- check

          | Pi Name Term Term     -- types

          | U

          deriving (Show, Eq)

type Context = [(Name, Term)]

extendCxt :: Name -> Term -> Context -> Context
extendCxt x t = ((x, t) :)

lookupCxt :: Name -> Context -> Term
lookupCxt = (maybe (error "Unbound variable") id .) . lookup

check :: (Applicative m, MonadState Int m, MonadError Err m)
      => Context -> Term -> Term -> m ()
check cxt e t = undefined

eval :: (Applicative m, MonadState Int m, MonadError Err m) => Term -> m Term
eval (Ap e1 e2) = do
  e1' <- eval e1
  e2' <- eval e2
  case e1' of
    Lam x e -> subst e e2' x
    _ -> return $ Ap e1' e2'
eval e@(Var x) = return e
eval (Lam x e) = Lam x <$> eval e
eval (Pi x e1 e2) = Pi x <$> eval e1 <*> eval e2
eval U = return U

{- mechanical operation on terms: equality test, substitution -}

equal :: Term -> Term -> Bool
equal = equalAux 0 Left Left

equalAux :: Int -> (Name -> Either String Int) -> (Name -> Either String Int)
         -> Term -> Term -> Bool
equalAux n cxt1 cxt2 (Var x) (Var y) = cxt1 x == cxt2 y
equalAux n cxt1 cxt2 (Ap e1 e2) (Ap e1' e2') =
  equalAux n cxt1 cxt2 e1 e1' && equalAux n cxt1 cxt2 e2 e2'
equalAux n cxt1 cxt2 (Lam x e) (Lam x' e') =
  equalAux (n+1) (extend cxt1 x n) (extend cxt2 x' n) e e'
  where extend cxt x n z = if x == z then Right n else cxt z
equalAux n cxt1 cxt2 (Pi x e1 e2) (Pi x' e1' e2') =
  equalAux n cxt1 cxt2 e1 e1' && equalAux (n+1) (extend cxt1 x n) (extend cxt2 x' n) e2 e2'
  where extend cxt x n z = if x == z then Right n else cxt z
equalAux n cxt1 cxt2 U U = True
equalAux n cxt1 cxt2 e1 e2 = False

-- `e1 [ e2 / x ]` is written as `subst e1 e2 x`
subst :: (Applicative m, MonadState Int m) => Term -> Term -> Name -> m Term
subst = substAux id

fresh :: (MonadState Int m) => Name -> m Name
fresh prefix = do
  n <- get
  modify (+1)
  return $ prefix `append` show n

substAux :: (Applicative m, MonadState Int m) => (Name -> Name) -> Term -> Term -> Name -> m Term
substAux cxt (Var y) e2 x
  | x == y                     = pure e2
  | otherwise                  = pure $ Var (cxt y)
substAux cxt (Ap e e') e2 x    = Ap <$> substAux cxt e e2 x <*> substAux cxt e' e2 x
substAux cxt e1@(Lam y e) e2 x
  | x == y                     = pure e1
  | otherwise                  = do
      y' <- fresh "x"
      Lam y' <$> substAux (\z -> if z == y then y' else cxt z) e e2 x
substAux cxt (Pi y e e') e2 x
  | x == y                     = Pi y <$> substAux cxt e e2 x <*> pure e'
  | otherwise                  = do
      y' <- fresh "T"
      Pi y' <$> substAux cxt e e2 x <*> substAux (\z -> if z == y then y' else cxt z) e' e2 x
substAux cxt U e2 x            = pure U

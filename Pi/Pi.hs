{-# LANGUAGE FlexibleContexts #-}

module Pi where

import Control.Applicative ((<$>), Applicative(..))
import Control.Monad
import Control.Monad.State
import Control.Monad.Except

typ1 :: Term
typ1 = Pi "A" U (Pi "x" (Var "A") (Var "A"))

expr1 :: Term
expr1 = Lam "A" (Lam "x" (Var "x"))

polyidCxt :: Term -> Term
polyidCxt = Let "polyid" typ1 expr1

expr2 :: Term
expr2 = polyidCxt $ (Var "polyid") `Ap` U `Ap` U

testCheck :: Term -> Term -> IO (Either Err ())
testCheck e t = evalStateT (runExceptT (check emptyCxt e t)) 100

type Err = String
type Name = String

append :: Name -> Name -> Name
append = (++)

data Term = Var Name              -- infer
          | Ap Term Term
          | Let Name Term Term Term -- let (x : t) = e1 in e2

          | Lam Name Term         -- check

          | Pi Name Term Term     -- types

          | U

          deriving (Show, Eq)

type Context = [(Name, Term)]

emptyCxt :: Context
emptyCxt = []

extendCxt :: Name -> Term -> Context -> Context
extendCxt x t = ((x, t) :)

lookupCxt :: Name -> Context -> Term
lookupCxt = (maybe (error "Unbound variable") id .) . lookup

check :: (Applicative m, MonadState Int m, MonadError Err m, MonadIO m)
      => Context -> Term -> Term -> m ()
check cxt e t = check' cxt e =<< eval cxt t

check' :: (Applicative m, MonadState Int m, MonadError Err m, MonadIO m)
      => Context -> Term -> Term -> m ()
check' cxt (Lam x e) (Pi t a b) = do -- The case (\x. e) : Pi_[t : a] b
  x' <- fresh x
  e' <- subst e (Var x') x -- e [ x' / x ]
  b' <- subst b (Var x') t -- b [ x' / t ]
  check cxt a U
  -- Shouldn't affect type checking; merely to speed up type checking
  -- Only do the evaluation had it type checked
  -- a' <- eval cxt a
  check (extendCxt x' a cxt) e' b'
check' cxt (Pi t a b) U = do
  check cxt a U
  check (extendCxt t a cxt) b U
check' cxt U U = return ()
check' cxt e t = do
  -- Any way to avoid this awkward check without duplicating the code?
  case e of
    Var _ -> return ()
    Lam _ _ -> return ()
    Let _ _ _ _ -> return ()
    _ -> throwError $ "check: missing case: '" ++ show e ++ "' : '" ++ show t ++ "'"
  t2 <- infer cxt e
  equ <- equal <$> eval cxt t <*> eval cxt t2
  if equ
    then return ()
    else throwError $ "check: type mismatch: " ++ show e ++ ": " ++ show t ++ " v.s. " ++ show t2

infer :: (Applicative m, MonadState Int m, MonadError Err m, MonadIO m)
      => Context -> Term -> m Term
infer cxt (Var x) = return $ lookupCxt x cxt
infer cxt (Ap e1 e2) = do -- elimination of Pi_[t : a] b
  t <- eval cxt =<< infer cxt e1
  case t of
    Pi x a b -> do
      check cxt e2 a
      subst b e2 x
    _ -> throwError $ "infer: application of non-function '" ++ show e1 ++ "': '" ++ show t ++ "'"
infer cxt (Let x t e1 e2) = do
  check cxt t U
  check cxt e1 t
  infer (extendCxt x e1 cxt) e2
infer cxt e = throwError $ "infer: missing case: '" ++ show e ++ "'"

eval :: (Applicative m, MonadState Int m, MonadError Err m)
     => Context -> Term -> m Term
eval cxt (Ap e1 e2) = do
  e1' <- eval cxt e1
  e2' <- eval cxt e2
  case e1' of
    Lam x e -> subst e e2' x
    _ -> return $ Ap e1' e2'
eval cxt (Let x t e1 e2) = subst e2 e1 x
eval cxt (Var x) =
  case lookup x cxt of
    Just e -> eval cxt e
    Nothing -> return (Var x)
eval cxt (Lam x e) = Lam x <$> eval cxt e
eval cxt (Pi x e1 e2) = Pi x <$> eval cxt e1 <*> eval cxt e2
eval cxt U = return U

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
      y' <- fresh y
      Lam y' <$> substAux (\z -> if z == y then y' else cxt z) e e2 x
substAux cxt (Pi y e e') e2 x
  | x == y                     = Pi y <$> substAux cxt e e2 x <*> pure e'
  | otherwise                  = do
      y' <- fresh y
      Pi y' <$> substAux cxt e e2 x <*> substAux (\z -> if z == y then y' else cxt z) e' e2 x
substAux cxt U e2 x            = pure U

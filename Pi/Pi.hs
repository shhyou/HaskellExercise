{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DeriveGeneric, TypeOperators, OverloadedStrings #-}

module Pi where

import qualified GHC.Exts (IsString(..))

import Data.Char (isAlpha)

import Text.PrettyPrint.GenericPretty

import Control.Applicative ((<$>), Applicative(..))
import Control.Monad
import Control.Monad.State
import Control.Monad.Except

polyidCxt :: Term -> Term
polyidCxt = Let "polyid" typ1 expr1

-- To pretty print: pp EXPR
expr1, typ1, expr2, typ2, expr3, typ3 :: Term

expr1 = Lam "A" (Lam "x" "x")
typ1 = Pi "A" U ("A" :=> "A")

expr2 = polyidCxt $ "polyid" :@ U :@ U
typ2 = U

expr3 = polyidCxt $ "polyid" :@ typ1 :@ "polyid"
typ3 = typ1

expr4 = Lam "id" $ "id" :@ Pi "a" U ("a" :=> "a") :@ "id"
typ4 = Pi "A" U ("A" :=> "A")   :=>   Pi "A" U ("A" :=> "A")

runM :: Monad m => ExceptT Err (StateT Int m) a -> m (Either Err a)
runM m = evalStateT (runExceptT m) 100

testCheck :: Term -> Term -> IO (Either Err ())
testCheck e t = runM (check emptyCxt e t)

testAll = pp =<< mapM (uncurry testCheck) [(expr1,typ1),(expr2,typ2),(expr3,typ3),(expr4,typ4)]

type Err = String
type Name = String

append :: Name -> Name -> Name
append = (++)

infixl 2 :@
infixr 1 :=>

data Term = Var Name              -- infer
          | Term :@ Term
          | Let Name Term Term Term -- let (x : t) = e1 in e2

          | Lam Name Term         -- check

          | Pi Name Term Term     -- types
          | Term :=> Term         -- (Pi _ A B)

          | U

          deriving (Show, Eq, Generic)

-- Automatic derived instance (from Generic)
instance Out Term

instance GHC.Exts.IsString Term where
  fromString = Var

type Context = [(Name, Term)]

emptyCxt :: Context
emptyCxt = []

extendCxt :: Name -> Term -> Context -> Context
extendCxt x t = ((x, t) :)

lookupCxt :: Name -> Context -> Term
lookupCxt = (maybe (error "Unbound variable") id .) . lookup

check :: (Applicative m, MonadState Int m, MonadError Err m, MonadIO m)
      => Context -> Term -> Term -> m ()
check cxt e t = check' cxt e =<< eval t

check' :: (Applicative m, MonadState Int m, MonadError Err m, MonadIO m)
       => Context -> Term -> Term -> m ()
check' cxt (Lam x e) (a :=> b) = do
  check cxt a U
  check cxt b U
  check (extendCxt x a cxt) e b
check' cxt (Lam x e) (Pi t a b) = do -- The case (\x. e) : Pi_[t : a] b
  x' <- fresh x
  e' <- subst e (Var x') x -- e [ x' / x ]
  b' <- subst b (Var x') t -- b [ x' / t ]
  check cxt a U
  -- Shouldn't affect type checking; merely to speed up type checking
  -- Only do the evaluation had it type checked
  -- a' <- eval a
  check (extendCxt x' a cxt) e' b'
check' cxt (a :=> b) U = do
  check cxt a U
  check cxt b U
check' cxt (Pi t a b) U = do
  check cxt a U
  check (extendCxt t a cxt) b U
check' cxt U U = return ()
check' cxt e t = do
  -- Any way to avoid this awkward check without duplicating the code?
  case e of
    Var _ -> return ()
    _ :@ _ -> return ()
    Let _ _ _ _ -> return ()
    _ -> throwError $ "check: missing case: '" ++ show e ++ "' : '" ++ show t ++ "'"
  t2 <- infer cxt e
  equ <- equal <$> eval t <*> eval t2
  unless equ $ throwError $ "check: type mismatch: " ++ show e ++ ": " ++ show t ++ " v.s. " ++ show t2

infer :: (Applicative m, MonadState Int m, MonadError Err m, MonadIO m)
      => Context -> Term -> m Term
infer cxt (Var x) = return $ lookupCxt x cxt
infer cxt (e1 :@ e2) = do -- elimination of Pi_[t : a] b
  t <- eval =<< infer cxt e1
  case t of
    Pi x a b -> do
      check cxt e2 a
      subst b e2 x
    _ -> throwError $ "infer: application of non-function '" ++ show e1 ++ "': '" ++ show t ++ "'"
infer cxt (Let x t e1 e2) = do
  check cxt t U
  check cxt e1 t
  infer (extendCxt x t cxt) e2
infer cxt e = throwError $ "infer: missing case: '" ++ show e ++ "'"

eval :: (Applicative m, MonadState Int m, MonadError Err m)
     => Term -> m Term
eval (e1 :@ e2) = do
  e1' <- eval e1
  e2' <- eval e2
  case e1' of
    Lam x e -> subst e e2' x
    _ -> return $ e1' :@ e2'
eval (Let x t e1 e2) = subst e2 e1 x
eval (Var x) = return (Var x)
eval (Lam x e) = Lam x <$> eval e
eval (e1 :=> e2) = (:=>) <$> eval e1 <*> eval e2
eval (Pi x e1 e2) = Pi x <$> eval e1 <*> eval e2
eval U = return U

{- mechanical operation on terms: equality test, substitution -}

equal :: Term -> Term -> Bool
equal = equalAux 0 Left Left

equalAux :: Int -> (Name -> Either String Int) -> (Name -> Either String Int)
         -> Term -> Term -> Bool
equalAux n cxt1 cxt2 (Var x) (Var y) = cxt1 x == cxt2 y
equalAux n cxt1 cxt2 (e1 :@ e2) (e1' :@ e2') =
  equalAux n cxt1 cxt2 e1 e1' && equalAux n cxt1 cxt2 e2 e2'
equalAux n cxt1 cxt2 (Lam x e) (Lam x' e') =
  equalAux (n+1) (extend cxt1 x n) (extend cxt2 x' n) e e'
  where extend cxt x n z = if x == z then Right n else cxt z
equalAux n cxt1 cxt2 (e1 :=> e2) (e1' :=> e2') =
  equalAux n cxt1 cxt2 e1 e1' && equalAux n cxt1 cxt2 e2 e2'
equalAux n cxt1 cxt2 (Pi x e1 e2) (Pi x' e1' e2') =
  equalAux n cxt1 cxt2 e1 e1' && equalAux (n+1) (extend cxt1 x n) (extend cxt2 x' n) e2 e2'
  where extend cxt x n z = if x == z then Right n else cxt z
equalAux n cxt1 cxt2 U U = True
equalAux n cxt1 cxt2 e1 e2 = False

-- `e1 [ e2 / x ]` is written as `subst e1 e2 x`
subst :: (Applicative m, MonadState Int m) => Term -> Term -> Name -> m Term
subst = substAux id

fresh :: (MonadState Int m) => Name -> m Name
fresh prefixNum = do
  n <- get
  modify (+1)
  return $ takeWhile isAlpha prefixNum `append` show n

substAux :: (Applicative m, MonadState Int m) => (Name -> Name) -> Term -> Term -> Name -> m Term
substAux cxt (Var y) e2 x
  | x == y                     = pure e2
  | otherwise                  = pure $ Var (cxt y)
substAux cxt (e :@ e') e2 x    = (:@) <$> substAux cxt e e2 x <*> substAux cxt e' e2 x
substAux cxt e1@(Lam y e) e2 x
  | x == y                     = pure e1
  | otherwise                  = do
      y' <- fresh y
      Lam y' <$> substAux (\z -> if z == y then y' else cxt z) e e2 x
substAux cxt (e :=> e') e2 x = (:=>) <$> substAux cxt e e2 x <*> substAux cxt e' e2 x
substAux cxt (Pi y e e') e2 x
  | x == y                     = Pi y <$> substAux cxt e e2 x <*> pure e'
  | otherwise                  = do
      y' <- fresh y
      Pi y' <$> substAux cxt e e2 x <*> substAux (\z -> if z == y then y' else cxt z) e' e2 x
substAux cxt U e2 x            = pure U

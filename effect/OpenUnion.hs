{-# LANGUAGE OverlappingInstances, GADTs, TypeOperators,
             KindSignatures, FlexibleContexts, FlexibleInstances,
             MultiParamTypeClasses #-}

module OpenUnion((:>), U(), (:&), inj, prj, decomp) where

import Data.Void -- only for Show U Void

infixr 1 :>
data ((a :: * -> *) :> b)

data U r v where
  Inl :: Functor t => t v -> U (t :> r) v
  Inr :: Functor t => U r v -> U (t :> r) v

instance Functor (U r) where
  fmap f (Inl tv) = Inl (fmap f tv)
  fmap f (Inr un) = Inr (fmap f un)
  
instance Show (U Void v) where
instance (Functor t, Show (t v), Show (U r v)) => Show (U (t :> r) v) where
  show (Inl x)  = "L(" ++ show x ++ ")"
  show (Inr tv) = "R" ++ show tv

class t :& r where
  inj :: Functor t => t v -> U r v
  prj :: Functor t => U r v -> Maybe (t v)

instance t :& (t :> r) where
  inj v = Inl v
  prj (Inl v) = Just v
  prj (Inr _) = Nothing

instance (Functor t', t :& r) => (t :& (t' :> r)) where
  inj v = Inr (inj v)
  prj (Inl _)  = Nothing
  prj (Inr tv) = prj tv

decomp :: Functor t => U (t :> r) v -> Either (U r v) (t v)
decomp (Inl v) = Right v
decomp (Inr u) = Left u

{-
x :: U (Either String :> Maybe :> Either Bool :> Void) Int
x = inj (Just 5)
-}

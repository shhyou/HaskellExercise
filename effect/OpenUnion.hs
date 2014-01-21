{-# LANGUAGE OverlappingInstances, GADTs, TypeOperators,
             KindSignatures, FlexibleContexts, FlexibleInstances,
             MultiParamTypeClasses #-}

module OpenUnion((:>), Union(), Member, inj, prj, decomp) where

import Data.Void -- only for Show Union Void

infixr 1 :>
data ((a :: * -> *) :> b)

data Union r v where
  Inl :: Functor t => t v -> Union (t :> r) v
  Inr :: Functor t => Union r v -> Union (t :> r) v

instance Functor (Union r) where
  fmap f (Inl tv) = Inl (fmap f tv)
  fmap f (Inr un) = Inr (fmap f un)
  
instance Show (Union Void v) where
instance (Functor t, Show (t v), Show (Union r v)) => Show (Union (t :> r) v) where
  show (Inl x)  = "L(" ++ show x ++ ")"
  show (Inr tv) = "R" ++ show tv

class Member t r where
  inj :: Functor t => t v -> Union r v
  prj :: Functor t => Union r v -> Maybe (t v)

instance Member t (t :> r) where
  inj v = Inl v
  prj (Inl v) = Just v
  prj (Inr _) = Nothing

instance (Functor t', Member t r) => Member t (t' :> r) where
  inj v = Inr (inj v)
  prj (Inl _)  = Nothing
  prj (Inr tv) = prj tv

decomp :: Functor t => Union (t :> r) v -> Either (Union r v) (t v)
decomp (Inl v) = Right v
decomp (Inr u) = Left u

{-
x :: Union (Either String :> Maybe :> Either Bool :> Void) Int
x = inj (Just 5)
-}

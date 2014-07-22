{-# LANGUAGE GADTs, RankNTypes, ScopedTypeVariables, DeriveFunctor
           , StandaloneDeriving, FlexibleContexts, FlexibleInstances
           , OverlappingInstances, MultiParamTypeClasses, KindSignatures
           , IncoherentInstances #-} -- evil

import Control.Monad (liftM)
import Control.Monad.Identity
import Control.Monad.Trans (MonadTrans(), lift)

data FreeT m f a where
  Pure :: a -> FreeT m f a
  Impure :: (f (m (FreeT m f a))) -> FreeT m f a

instance (Monad m, Functor f) => Monad (FreeT m f) where
  return = Pure
  (Pure x) >>= k   = k x
  (Impure f) >>= k = Impure (fmap (liftM (>>= k)) f)

liftF :: (Monad m, Functor f) => f a -> FreeT m f a
liftF = Impure . fmap (return . Pure)

data TickF p a where
  Tick :: a -> TickF p a
  Count :: (Int -> a) -> TickF p a

data Named p = Named

data Either1 a b t = Left1 (a t) | Right1 (b t)

class    Functor fs => Mem f (fs :: * -> *) where inj :: f a -> fs a
instance Mem (TickF p) (Either1 (TickF p) (TickF q)) where inj = Left1
instance Mem (TickF p) (Either1 (TickF q) (TickF p)) where inj = Right1

tick :: forall m p fs. (Monad m, Mem (TickF p) fs) => Named p -> FreeT m fs ()
tick Named = liftF (inj (Tick () :: TickF p ()))

count :: forall m p fs. (Monad m, Mem (TickF p) fs) => Named p -> FreeT m fs Int
count Named = liftF (inj (Count id :: TickF p Int))

test1 :: (Monad m, Mem (TickF p) fs) => Named p -> FreeT m fs Int
test1 cnt = do
  () <- tick cnt
  () <- tick cnt
  () <- tick cnt
  n <- count cnt
  return (n*2 + 1)

test2 :: (Monad m, Mem (TickF p) fs) => Named p -> FreeT m fs Int
test2 cnt = do
  () <- tick cnt
  count cnt

tests :: (Monad m, Mem (TickF p) fs, Mem (TickF q) fs)
      => Named p
      -> Named q
      -> FreeT m fs (Int, Int)
tests cnt1 cnt2 = do
  test1 cnt1
  test2 cnt2
  n1 <- count cnt1
  n2 <- count cnt2
  return (n1, n2)

runTicks :: Monad m
        => (forall p q. Named p -> Named q -> FreeT m (Either1 (TickF p) (TickF q)) a)
        -> m a
runTicks f = loop 0 0 (f Named Named) where
  loop nl nr (Pure x) = return x
  loop nl nr (Impure (Left1   (Tick k))) = k >>= loop (nl + 1) nr
  loop nl nr (Impure (Left1  (Count k))) = k nl >>= loop nl nr
  loop nl nr (Impure (Right1  (Tick k))) = k >>= loop nl (nr + 1)
  loop nl nr (Impure (Right1 (Count k))) = k nr >>= loop nl nr

res :: (Int, Int)
res = runIdentity (runTicks tests)

deriving instance (Functor a, Functor b) => Functor (Either1 a b)
deriving instance Functor (TickF p)
deriving instance (Functor m, Functor f) => Functor (FreeT m f)

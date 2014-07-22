{-# LANGUAGE GADTs, RankNTypes, ScopedTypeVariables, DeriveFunctor, StandaloneDeriving #-}

import Control.Monad (liftM)
import Control.Monad.Identity

data FreeT m f a where
  Pure' :: a -> FreeT m f a
  Impure :: (f (m (FreeT m f a))) -> FreeT m f a

instance (Monad m, Functor f) => Monad (FreeT m f) where
  return = Pure'
  (Pure' x) >>= k   = k x
  (Impure f) >>= k = Impure (fmap (liftM (>>= k)) f)

data TickF m a where
  Pure :: a -> TickF m a
  Tick :: m (TickF m a) -> TickF m a
  Count :: (Int -> m (TickF m a)) -> TickF m a

instance Monad m => Monad (TickF m) where
  return = Pure
  (Pure x)  >>= k = k x
  (Tick m)  >>= k = Tick (liftM (>>= k) m)
  (Count f) >>= k = Count ((liftM (>>= k)) . f)

runTick :: Monad m => TickF m a -> m a
runTick = runTick' 0 where
  runTick' :: Monad m => Int -> TickF m a -> m a
  runTick' c (Pure x)  = return x
  runTick' c (Tick m)  = m >>= runTick' (c+1)
  runTick' c (Count f) = f c >>= runTick' c

tick :: Monad m => TickF m ()
tick = Tick (return $ Pure ())

count :: Monad m => TickF m Int
count = Count (return . Pure)

test1 :: Monad m => TickF m Int
test1 = do
  () <- tick
  () <- tick
  () <- tick
  n <- count
  return (n*2 + 1)

deriving instance Functor m => Functor (TickF m)
deriving instance (Functor m, Functor f) => Functor (FreeT m f)

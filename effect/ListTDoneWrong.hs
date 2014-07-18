import Control.Monad.Trans
import Control.Monad.Identity

newtype ListT m a = ListT { runListT :: m [a] }
wrap a = [a]
instance Monad m => Monad (ListT m) where
  return = ListT . return . wrap
  (ListT m) >>= f = ListT (m >>= mapM (runListT . f) >>= return . concat)
instance MonadTrans ListT where
  lift m = ListT (m >>= return . wrap)

newtype IntStateT m a = IntStateT { runIntStateT :: Int -> m (a, Int) }
instance Monad m => Monad (IntStateT m) where
  return a = IntStateT (\s -> return (a, s))
  (IntStateT m) >>= f = IntStateT $ \s -> do
    (a, s') <- m s
    runIntStateT (f a) s'
instance MonadTrans IntStateT where
  lift m = IntStateT $ \s -> do
    a <- m
    return (a, s)
get = IntStateT (\s -> return (s, s))
modify f = IntStateT (\s -> return ((), f s))

comb0 :: ListT (IntStateT Identity) Int
comb0 = do
  ListT (return [0..9])
  lift (modify (+1))
  lift get

comb1 :: ListT (IntStateT Identity) Int
comb1 = do
  do ListT (return [0..9])
     lift (modify (+1))
  lift get

runs m = runIdentity (runIntStateT (runListT m) 0)
vals = map runs [comb0, comb1]

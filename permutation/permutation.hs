import Control.Applicative
import Control.Monad
import Control.Monad.Identity
import Data.List ((\\))

-- list version
permutation :: [Int] -> [[Int]]
permutation [] = [[]]
permutation xs = do
  x <- xs
  ps <- permutation (xs \\ [x])
  return (x:ps)

-- generator (coroutine?) version permutation
data Yield b m a =
    Done a
  | Yield b (m (Yield b m a))

yield :: Monad m => b -> Yield b m ()
yield b = Yield b (return (Done ()))

permutationAcc :: Monad m => [Int] -> [Int] -> Yield [Int] m ()
permutationAcc xs acc =
  if null xs
  then yield (reverse acc)
  else forM_ xs $ \x ->
         permutationAcc (xs \\ [x]) (x:acc)

testPermutation :: [Int] -> IO ()
testPermutation xs = loop (permutationAcc xs []) where
  loop (Done a) = do
    putStr "Done "
    print a
  loop (Yield b m) = do
    print b
    loop (runIdentity m)

instance Monad m => Functor (Yield b m) where
  fmap f (Done a) = Done (f a)
  fmap f (Yield b m) = Yield b (liftM (fmap f) m)

-- Applicative will be a super class of Monad in GHC 7.10
instance Monad m => Applicative (Yield b m) where
  pure = Done
  (Done f) <*> (Done x) = Done (f x)
  (Done f) <*> (Yield b m) = Yield b (liftM (fmap f) m)
  (Yield b m) <*> x = Yield b (liftM (<*> x) m)

instance Monad m => Monad (Yield b m) where
  return = Done
  (Done a)    >>= f = f a
  (Yield b m) >>= f = Yield b (liftM (>>= f) m)

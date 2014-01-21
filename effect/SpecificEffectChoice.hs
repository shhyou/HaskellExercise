{-# LANGUAGE RankNTypes, GADTs #-}

module SpecificEffectChoice where

newtype Eff a = Eff { runEff :: forall w. (a -> VE w) -> VE w }
instance Monad Eff where
  return x = Eff $ \k -> k x
  m >>= f  = Eff $ \k -> runEff m (\a -> runEff (f a) k)

data VE w = Val w | forall a. E [a] (a -> VE w)

nondeterministicChoice :: [a] -> Eff a
nondeterministicChoice xs = Eff $ \k -> E xs k

admin :: Eff w -> VE w
admin m = runEff m Val

runChoice :: Eff w -> [w]
runChoice m = loop (admin m)
  where loop :: VE w -> [w]
        loop (Val x)  = [x]
        loop (E xs k) = concat $ map (loop . k) xs

testC = do
  x <- nondeterministicChoice [1..3]
  y <- nondeterministicChoice [1..4]
  return (x,y)

testCP = runChoice test

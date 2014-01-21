{-# LANGUAGE RankNTypes #-}

module SpecificEffectReader where

newtype Eff a = Eff { runEff :: forall w. (a -> VE w) -> VE w }
instance Monad Eff where
  return x = Eff $ \k -> k x
  m >>= f  = Eff $ \k -> runEff m (\a -> runEff (f a) k)

data VE w = Val w | E (Int -> VE w)

ask :: Eff Int
ask = Eff (\k -> E k)

local :: (Int -> Int) -> Eff Int -> Eff Int
local f m = do
  e <- ask
  let e' = f e
  let loop (Val x) = return x
      loop (E k)   = loop (k e')
  loop (admin m)

admin :: Eff w -> VE w
admin m = runEff m Val

runReader :: Eff w -> Int -> w
runReader m e = loop (admin m)
  where loop :: VE w -> w
        loop (Val x) = x
        loop (E k)   = loop (k e)

testR = do
  n <- ask
  return $ 3 * n + 1

testRP = runReader test 5

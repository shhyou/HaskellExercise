{-# LANGUAGE RankNTypes #-}

module SpecificEffectState where

newtype Eff a = Eff { runEff :: forall w. (a -> VE w) -> VE w }
instance Monad Eff where
  return x = Eff $ \k -> k x
  m >>= f  = Eff $ \k -> runEff m (\a -> runEff (f a) k)

data VE w = Val w | E (State (VE w))

data State v = Get ([Int] -> v)
             | Put [Int] (() -> v)

get :: Eff [Int]
get = Eff (\k -> E (Get k))

put :: [Int] -> Eff ()
put s = Eff (\k -> E (Put s k))

admin :: Eff w -> VE w
admin m = runEff m Val

runState :: [Int] -> Eff w -> w
runState s m = loop s (admin m)
  where loop :: [Int] -> VE w -> w
        loop _ (Val x)        = x
        loop s (E (Get k))    = loop s (k s)
        loop s (E (Put s' k)) = loop s' (k ())

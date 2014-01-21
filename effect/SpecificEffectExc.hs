{-# LANGUAGE RankNTypes #-}

module SpecificEffectExc where

newtype Eff a = Eff { runEff :: forall w. (a -> VE w) -> VE w }
instance Monad Eff where
  return x = Eff $ \k -> k x
  m >>= f  = Eff $ \k -> runEff m (\a -> runEff (f a) k)

data VE w = Val w | E String

throwError :: String -> Eff a
throwError msg = Eff $ \_ -> E msg

admin :: Eff w -> VE w
admin m = runEff m Val

runError :: Eff w -> Either String w
runError m = loop (admin m)
  where loop :: VE w -> Either String w
        loop (Val x) = Right x
        loop (E msg) = Left msg

testE m = do
  if (m > 10)
    then throwError "m > 10"
    else return True
  return ()

testEP m = runError (test m)

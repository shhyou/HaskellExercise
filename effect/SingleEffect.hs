{-# LANGUAGE RankNTypes, GADTs, ScopedTypeVariables #-}

module SingleEffect where

import Control.Monad (liftM2)

newtype Eff r a = Eff { runEff :: forall w. (a -> VE w r) -> VE w r}
instance Monad (Eff r) where
  return x = Eff $ \k -> k x
  m >>= f  = Eff $ \k -> runEff m (\a -> runEff (f a) k)

data VE w r = Val w | E (r (VE w r))

sendReq :: (forall w. (a -> VE w r) -> r (VE w r)) -> Eff r a
sendReq f = Eff $ \k -> E (f k)

admin :: Eff r w -> VE w r
admin m = runEff m Val

-- pure comp
data Void v
run :: Eff Void w -> w
run m = case admin m of
          Val x -> x

{- Reader -}
newtype Reader e v = Reader (e -> v)

ask :: Eff (Reader e) e
ask = sendReq Reader
-- Reader :: forall w. (e -> VE w (Reader e)) -> Reader e (VE w (Reader e))

-- forall e w. to explicitly bind `e`, `w`, for the type of `loop`
runReader :: forall e w. Eff (Reader e) w -> e -> Eff Void w
runReader m e = loop (admin m)
  where loop :: VE w (Reader e) -> Eff Void w
        loop (Val x)        = return x
        loop (E (Reader k)) = loop (k e)


{- Error -}
newtype Error e v = Error e

throwError :: e -> Eff (Error e) v
throwError msg = sendReq (\_ -> Error msg)

runError :: forall e w. Eff (Error e) w -> Eff Void (Either e w)
runError m = loop (admin m)
  where loop :: VE w (Error e) -> Eff Void (Either e w)
        loop (Val x)       = return (Right x)
        loop (E (Error e)) = return (Left e)


{- Choice -}
data Choice v = forall w. Choice [w] (w -> v)

nondeterministicChoice :: [a] -> Eff Choice a
nondeterministicChoice xs = sendReq (Choice xs)

runChoice :: forall a. Eff Choice a -> Eff Void [a]
runChoice m = loop (admin m)
  where loop :: VE a Choice -> Eff Void [a]
        loop (Val x)           = return [x]
        loop (E (Choice xs k)) = foldr (liftM2 (++)) (return []) $ map (loop . k) xs

{- tests -}
testR = do
  n <- ask
  return $ 3 * n + 1

testRP = run $ runReader testR 5

testE m = do
  if (m > 10)
    then throwError "m > 10"
    else return True
  return ()

testEP m = run $ runError (testE m)

testC = do
  x <- nondeterministicChoice [1..3]
  y <- nondeterministicChoice [1..4]
  return (x,y)

testCP = run $ runChoice testC

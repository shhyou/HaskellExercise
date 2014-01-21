{-# LANGUAGE RankNTypes, GADTs, ScopedTypeVariables, TypeOperators,
             FlexibleContexts, FlexibleInstances #-}

module Effect (Void, module OpenUnion, module Effect) where

import OpenUnion
import Data.Void
import Control.Monad (liftM2)

newtype Eff r a = Eff { runEff :: forall w. (a -> VE w r) -> VE w r}
instance Monad (Eff r) where
  return x = Eff $ \k -> k x
  m >>= f  = Eff $ \k -> runEff m (\a -> runEff (f a) k)
instance Functor (Eff r) where
  fmap f m = Eff (runEff m . (. f))

data VE w r = Val w | E (Union r (VE w r))

sendReq :: (forall w. (a -> VE w r) -> Union r (VE w r)) -> Eff r a
sendReq f = Eff $ \k -> E (f k)

admin :: Eff r w -> VE w r
admin m = runEff m Val

run :: Eff Void w -> w
run m = case admin m of
          Val x -> x

{- Reader -}
newtype Reader e v = Reader (e -> v)
instance Functor (Reader e) where
  fmap f (Reader g) = Reader (f . g)

ask :: Member (Reader e) r => Eff r e
ask = sendReq (inj . Reader)

runReader :: forall r w e. Eff (Reader e :> r) w -> e -> Eff r w
runReader m e = loop (admin m)
  where loop :: VE w (Reader e :> r) -> Eff r w
        loop (Val x) = return x
        loop (E un)  = case decomp un of
          Right (Reader k) -> loop (k e)
          Left u -> Eff $ \k -> E (fmap (\ve -> runEff (loop ve) k) u)
        --       -> sendReq (<$> u) >>= loop

local :: forall e r. Member (Reader e) r => (e -> e) -> Eff r e -> Eff r e
local f m = do
  e0 <- ask
  let e = f e0
  let loop :: VE w r -> Eff r w
      loop (Val x) = return x
      loop (E un)  = case prj un of
        Just (Reader k) -> loop (k e)
        Nothing -> sendReq (\k -> fmap k un) >>= loop
  loop (admin m)
        --         we want an Eff r (VE w r)     loop, process all requests
-- now we have only  un :: Union r (VE w r)
-- sendReq :: (forall t. (VE w r -> VE t r) -> Union r (VE t r)) -> Eff r (VE w r)
--                       ^~~~~~~~~~~ (k ::)   ^~~~~ (fmap k un ::)

{- Exception -}
newtype Error e v = Error e
instance Functor (Error e) where
  fmap _ (Error e) = (Error e)

throwError :: Member (Error e) r => e -> Eff r a
throwError msg = sendReq (inj . const (Error msg))

catchError :: Member (Error e) r => Eff r a -> (e -> Eff r a) -> Eff r a
catchError m handler = loop (admin m)
  where loop (Val x) = return x
        loop (E un)  = case prj un of
          Just (Error e) -> handler e
          Nothing        -> sendReq (\k -> fmap k un) >>= loop

runError :: forall r w e. Eff (Error e :> r) w -> Eff r (Either e w)
runError m = loop (admin m)
  where loop :: VE w (Error e :> r) -> Eff r (Either e w)
        loop (Val x) = return (Right x)
        loop (E un)  = case decomp un of
          Right (Error e) -> return (Left e)
          Left u -> sendReq (\k -> fmap k u) >>= loop

{- Nondeterminism -}
data Choice v = forall a. Choice [a] (a -> v)
instance Functor Choice where
  fmap f (Choice xs k) = Choice xs (f . k)

choice :: Member Choice r => [a] -> Eff r a
choice xs = sendReq (inj . Choice xs)

runChoice :: forall r a. Eff (Choice :> r) a -> Eff r [a]
runChoice m = loop (admin m)
  where loop :: VE a (Choice :> r) -> Eff r [a]
        loop (Val x) = return [x]
        loop (E un)  = case decomp un of
          Right (Choice xs k) -> foldr (liftM2 (++)) (return []) $ map (loop . k) xs
          Left u -> sendReq (\k -> fmap k u) >>= loop

{- mutable var -}
data State s v = GetState (s -> v)
               | SetState s (() -> v)
instance Functor (State s) where
  fmap f (GetState k) = GetState (f . k)
  fmap f (SetState s k) = SetState s (f . k)

get :: Member (State s) r => Eff r s
get = sendReq (inj . GetState)

put :: Member (State s) r => s -> Eff r ()
put s = sendReq (inj . SetState s)

modify :: forall r s. Member (State s) r => (s -> s) -> Eff r ()
modify f = do
  state :: s <- get
  put (f state)

runState :: forall s r a. s -> Eff (State s :> r) a -> Eff r a
runState initVal m = loop initVal (admin m)
  where loop :: forall w. s -> VE w (State s :> r) -> Eff r w
        loop val (Val x) = return x
        loop val (E un) = case decomp un of
          Right (GetState k) -> loop val (k val)
          Right (SetState newVal k) -> loop newVal (k ())
          Left u -> sendReq (\k -> fmap k u) >>= loop val

runState' :: forall s r a. s -> Eff (State s :> r) a -> Eff r (a, s)
runState' initVal m = runState initVal (liftM2 (,) m get)

{-
data Delim v = forall a r w. Reset (VE a r) (a -> VE w r)

reset :: Member r => Eff r a -> Eff r a
reset (Eff k) = sendReq (inj . Reset (admin k))
-}
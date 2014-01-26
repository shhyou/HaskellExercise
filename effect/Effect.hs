{-# LANGUAGE RankNTypes, GADTs, ScopedTypeVariables, TypeOperators,
             FlexibleContexts, FlexibleInstances #-}

module Effect (Void, module OpenUnion, module Effect) where

import OpenUnion
import Data.Void
import Control.Monad (mapM, liftM2, liftM)

newtype Eff r a = Eff { runEff :: forall w. (a -> VE w r) -> VE w r}
instance Monad (Eff r) where
  return x = Eff $ \k -> k x
  m >>= f  = Eff $ \k -> runEff m (\a -> runEff (f a) k)
instance Functor (Eff r) where
  fmap f m = Eff (runEff m . (. f))

data VE w r = Val w | E (U r (VE w r))

sendReq :: (forall w. (a -> w) -> U r w) -> Eff r a
sendReq f = Eff $ \k -> E (f k)

admin :: Eff r w -> VE w r
admin m = runEff m Val

run :: Eff Void w -> w
run m = case admin m of
          Val x -> x

withHandler :: Functor t
            => (a -> Eff rs w)
            -> (t (VE a (t :> rs)) -> Eff rs w)
            -> VE a (t :> rs)
            -> Eff rs w
withHandler valHandler effHandler =
  let handler = \val -> case val of
                  Val x -> valHandler x
                  E un  -> case decomp un of
                    Right tv -> effHandler tv
                    Left u -> sendReq (\k -> fmap k u) >>= handler
  in handler

insertHandler :: (Functor t, t :& rs)
              => (a -> Eff rs w)
              -> (t (VE a rs) -> Eff rs w)
              -> VE a rs
              -> Eff rs w
insertHandler valHandler effHandler =
  let handler = \val -> case val of
                  Val x -> valHandler x
                  E un -> case prj un of
                    Just tv -> effHandler tv
                    Nothing -> sendReq (\k -> fmap k un) >>= handler
  in handler

{- Reader -}
newtype Reader e v = Reader (e -> v)
instance Functor (Reader e) where
  fmap f (Reader g) = Reader (f . g)

ask :: (Reader e :& r) => Eff r e
ask = sendReq (inj . Reader)

runReader :: forall r w e. Eff (Reader e :> r) w -> e -> Eff r w
runReader m e = handler (admin m)
  where handler = withHandler return (\(Reader k) -> handler (k e))

local :: forall e r. (Reader e :& r) => (e -> e) -> Eff r e -> Eff r e
local f m = do
  e0 <- ask
  let e = f e0
      handler = insertHandler return (\(Reader k) -> handler (k e))
  handler (admin m)

{- Exception -}
newtype Error e v = Error e
instance Functor (Error e) where
  fmap _ (Error e) = (Error e)

throwError :: (Error e :& r) => e -> Eff r a
throwError msg = sendReq (inj . const (Error msg))

catchError :: (Error e :& r) => Eff r a -> (e -> Eff r a) -> Eff r a
catchError m onError = handler (admin m)
  where handler = insertHandler return (\(Error e) -> onError e)

runError :: forall r w e. Eff (Error e :> r) w -> Eff r (Either e w)
runError m = handler (admin m)
  where handler = withHandler (return . Right) (\(Error e) -> return (Left e))

{- Nondeterminism -}
data Choice v = forall a. Choice [a] (a -> v)
instance Functor Choice where
  fmap f (Choice xs k) = Choice xs (f . k)

choice :: (Choice :& r) => [a] -> Eff r a
choice xs = sendReq (inj . Choice xs)

runChoice :: forall r a. Eff (Choice :> r) a -> Eff r [a]
runChoice m = handler (admin m)
  where handler = withHandler wrap collect
        wrap x = return [x]
        collect (Choice xs k) = liftM concat $ mapM (handler . k) xs

{- mutable var -}
data State s v = GetState (s -> v)
               | SetState s (() -> v)
instance Functor (State s) where
  fmap f (GetState k) = GetState (f . k)
  fmap f (SetState s k) = SetState s (f . k)

get :: (State s :& r) => Eff r s
get = sendReq (inj . GetState)

put :: (State s :& r) => s -> Eff r ()
put s = sendReq (inj . SetState s)

modify :: (State s :& r) => (s -> s) -> Eff r ()
modify f = put . f =<< get

runState :: forall s r a. s -> Eff (State s :> r) a -> Eff r a
runState initVal m = handler (state initVal) (admin m)
  where handler = withHandler return
        state s (GetState    k) = handler (state s) (k s)
        state _ (SetState s' k) = handler (state s') (k ())

runState' :: forall s r a. s -> Eff (State s :> r) a -> Eff r (a, s)
runState' initVal m = runState initVal (liftM2 (,) m get)

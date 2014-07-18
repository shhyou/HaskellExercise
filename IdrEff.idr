module IdrEff

{-
Core: An *parameterised* data type, carrying resources, hence:
  - Effect: The type describing an computation
  - EFFECT: The type of the computation together with its resource type
  - Env:    The resource type list
-}

Effect : Type
Effect = (a : Type) -> (resTy : Type) -> (kResTy : a -> Type) -> Type

data EFFECT : Type where
  MkEff : (resTy : Type) -> (effTy : Effect) -> EFFECT

class EffHandler (effTy : Effect) (m : Type -> Type) where
  handle : (res : resTy)
        -> (eff : effTy a resTy resTyk)
        -> (k : (val : a) -> resTyk val -> m r)
        -> m r

namespace Env
  data Env : (m : Type -> Type) -> List EFFECT -> Type where
    Nil  : Env m Nil
    (::) : EffHandler effTy m
        => (res : resTy)
        -> Env m effs
        -> Env m (MkEff resTy effTy::effs)

data EffElem : (resTy : Type) -> (effTy : Effect) -> List EFFECT -> Type where
  Here  : EffElem resTy effTy (MkEff resTy effTy::effs)
  There : (p : EffElem resTy effTy effs)
       -> EffElem resTy effTy (eff::effs)

{-
we did not pass in the updated resource type directly,
since doing so will lost the chance to check its correctness.
Here we are sure that the type is correct via `eff : effTy a resTy resTyk`
-}
updateResTy : (val : a)
           -> (effs : List EFFECT)
           -> (prf : EffElem resTy effTy effs)
           -> (eff : effTy a resTy resTyk)
           -> List EFFECT
updateResTy {resTyk} val (MkEff resTy effTy::es) Here eff =
  MkEff (resTyk val) effTy::es
updateResTy val (e::es) (There p) eff =
  e::updateResTy val es p eff

data Eff  : (m : Type -> Type)
         -> (a : Type)
         -> (effs : List EFFECT)
         -> (effsk : a -> List EFFECT)
         -> Type where
  evalue  : a -> Eff m a effs (\val => effs)
  ebind   : Eff m a effs effsk
         -> ((val : a) -> Eff m b (effsk val) effsk')
         -> Eff m b effs effsk'
  einvoke : (prf : EffElem resTy effTy effs)
         -> (eff : effTy a resTy resTyk)
         -> Eff m a effs (\val => updateResTy val effs prf eff)

effexec : Env m effs
       -> (prf : EffElem resTy effTy effs)
       -> (eff : effTy a resTy resTyk)
       -> ((val : a) -> Env m (updateResTy val effs prf eff) -> m r)
       -> m r
effexec (res::ress) Here eff k =
  handle res eff (\val, res' => k val (res'::ress))
effexec (res::ress) (There p) eff k =
  effexec ress p eff (\val, ress' => k val (res::ress'))

effint : Env m effs
      -> Eff m a effs effsk
      -> ((val : a) -> Env m (effsk val) -> m r)
      -> m r
effint env (evalue a) k =
  k a env
effint env (ebind m f) k =
  effint env m (\val, env' => effint env' (f val) k)
effint env (einvoke prf eff) k =
  effexec env prf eff k

---------------------------- effect tests ---------------------------------

return : a -> Eff m a effs (\val => effs)
return = evalue

(>>=) : Eff m a effs effsk
     -> ((val : a) -> Eff m b (effsk val) effsk')
     -> Eff m b effs effsk'
(>>=) = ebind

data State : Effect where
  Get : State s s (\s_ => s)
  Put : s -> State () s (\s_ => s)

STATE : Type -> EFFECT
STATE s = MkEff s State

instance EffHandler State m where
  handle s Get      k = k s  s
  handle s (Put s') k = k () s'

get : Eff m s [STATE s] (\val => [STATE s])
get = einvoke Here Get

put : s -> Eff m () [STATE s] (\val => [STATE s])
put s = einvoke Here (Put s)

testState : Eff m Int [STATE Int] (\val => [STATE Int])
testState = do
  put 5
  n <- get
  put (n * 3)
  return (n+1)

runTest : (Show s, Show a) => Eff IO a [STATE s] (\val => [STATE s]) -> s -> IO a
runTest eff s = effint [s] eff (\a, envs => do
                  putStr "Computation ended with state "
                  let [s'] = envs
                  print s'
                  putStr "Value: "
                  print a
                  return a)

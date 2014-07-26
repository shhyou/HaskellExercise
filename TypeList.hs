{-# LANGUAGE MultiParamTypeClasses, FlexibleContexts, FlexibleInstances
           , UndecidableInstances, TypeFamilies, DataKinds, KindSignatures
           , GADTs, TypeOperators, ScopedTypeVariables, RankNTypes #-}

data Nat = Z | S Nat

type family Lookup (f :: * -> *) (fs :: [* -> *]) :: Nat where
  Lookup f (f ': fs) = Z
  Lookup f (g ': fs) = S (Lookup f fs)

data Proxy (n :: Nat) = Proxy

data Fmaps (fs :: [* -> *]) where
  Nil :: Fmaps '[]
  Cons :: (forall a b. f a -> (a -> b) -> f b) -> Fmaps fs -> Fmaps (f ': fs)

class Col (n :: Nat) (f :: * -> *) (fs :: [* -> *]) where
  getCon' :: Proxy n -> Fmaps fs -> f a -> (a -> b) -> f b

instance Col Z f (f ': fs) where
  getCon' _ (Cons f _) = f

instance Col n f fs => Col (S n) f (g ': fs) where
  getCon' _ (Cons _ fs) = getCon' (Proxy :: Proxy n) fs

class Fmappable (f :: * -> *) (fs :: [* -> *]) where
  getCon :: Fmaps fs -> f a -> (a -> b) -> f b

instance Col (Lookup f fs) f fs => Fmappable f fs where
  getCon = getCon' (Proxy :: Proxy (Lookup f fs))

data FmapSrc (fs :: [* -> *]) (a :: *) where
  FmapSrc :: Fmappable f fs => f a -> FmapSrc fs a

fmapFmaps :: forall fs a b. Fmaps fs -> (a -> b) -> FmapSrc fs a -> FmapSrc fs b
fmapFmaps fmaps g (FmapSrc fa) = FmapSrc (getCon fmaps fa g)

data Pure a = Pure { impure :: a }

mapPure :: Pure a -> (a -> b) -> Pure b
mapPure (Pure a) f = Pure (f a)

injPure :: Fmappable Pure fs => a -> FmapSrc fs a
injPure = FmapSrc . Pure

data Option a = None | Some a

mapOption :: Option a -> (a -> b) -> Option b
mapOption (Some a) f = Some (f a)
mapOption None     _ = None

injSome :: Fmappable Option fs => a -> FmapSrc fs a
injSome = FmapSrc . Some

injNone :: Fmappable Option fs => FmapSrc fs a
injNone = FmapSrc None

fmaps :: Fmaps '[Pure, Option]
fmaps = Cons mapPure (Cons mapOption Nil)

test1 :: FmapSrc '[Pure, Option] String
test1 = fmapFmaps fmaps (show :: Int -> String) (injPure 5)

test2 :: FmapSrc '[Pure, Option] String
test2 = fmapFmaps fmaps (show :: Int -> String) (injSome 5)

test3 :: FmapSrc '[Pure, Option] String
test3 = fmapFmaps fmaps (show :: Int -> String) injNone

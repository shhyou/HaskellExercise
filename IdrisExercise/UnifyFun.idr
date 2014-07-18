data Id : (A : Type) -> A -> A -> Type where
  Refl : {A : Type} -> (x : A) -> Id A x x

data Fn : (Nat -> Type) -> Type where
  MkFn : Nat -> Fn (\n => Id Nat n n)

fn : a -> Nat -> Fn (\n => Id Nat n n)
fn = const MkFn

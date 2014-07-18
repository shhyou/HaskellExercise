module DataType

data Wrap : Nat -> Type where
  W : (k : Nat) -> Wrap k

trans : Wrap n -> Wrap n -> ()
trans {n} wrapped (W n) = ()

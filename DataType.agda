module DataType where

data Nat : Set where
  Z : Nat
  S : Nat → Nat

data Wrap : Nat → Set where
  W : (k : Nat) → Wrap k

data ⊤ : Set where
  unit : ⊤

trans : ∀{ n } → Wrap n → Wrap n → ⊤
trans wrapped (W n) = unit

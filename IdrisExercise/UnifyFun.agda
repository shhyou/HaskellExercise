module UnifyFun where

data Nat : Set where
  Z : Nat
  S : Nat → Nat

data Id (A : Set) : A → A → Set where
  Refl : (x : A) → Id A x x

data Fn : (Nat → Set) → Set where
  MkFn : Nat → Fn (λ n → Id Nat n n)

fn : ∀{a : Set} → a → Nat → Fn (λ n → Id Nat n n)
fn _ = MkFn

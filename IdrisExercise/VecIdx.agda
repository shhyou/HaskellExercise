module VecIdx where

data ℕ : Set where
  zero : ℕ
  suc : ℕ → ℕ

{-# BUILTIN NATURAL ℕ #-}

data Fin : ℕ → Set where
  fzero : ∀{n} → Fin (suc n)
  fsuc : ∀{n} → Fin n → Fin (suc n)

data Vec (A : Set) : ℕ → Set where
  [] : Vec A 0
  _∷_ : ∀{n} → A → Vec A n → Vec A (suc n)

data _×_ (A : Set) (B : Set) : Set where
  _,_ : A → B → A × B

data _≡_ {A : Set} (a : A) : A → Set where
  refl : a ≡ a

data ⊥ : Set where

⊥-elim : {A : Set} → ⊥ → A
⊥-elim ()

map : ∀{A B n} → (A → B) → Vec A n → Vec B n
map f [] = []
map f (x ∷ xs) = f x ∷ map f xs

second : ∀{A B C} → (B → C) → A × B → A × C
second f (x , y) = (x , f y)

addIndex : ∀{A n} → Vec A n → Vec (A × Fin n) n
addIndex [] = []
addIndex (x ∷ xs) = (x , fzero) ∷ map (second fsuc) (addIndex xs)

_!_ : ∀{A n} → Vec A n → Fin n → A
[] ! ()
(x ∷ xs) ! fzero = x
(x ∷ xs) ! fsuc n = xs ! n

snd : ∀{A B} → A × B → B
snd (_ , y) = y

index-is-good : ∀{A n} {m : Fin n} {xs : Vec A n} → snd (addIndex xs ! m) ≡ m
index-is-good {n = zero} {()} {xs = []}
index-is-good {n = suc k} {fzero} {x ∷ xs} = refl
index-is-good {n = suc k} {fsuc m} {x ∷ xs} = {!index-is-good {n = k} {m = m} {xs = xs}!}

tabulate : ∀{A n} → (f : Fin n → A) → Vec A n
tabulate {n = zero} f = []
tabulate {n = suc _} f = f fzero ∷ tabulate (λ m → f (fsuc m))

_≢_ : {A : Set} → (x : A) → (y : A) → Set
x ≢ y = x ≡ y → ⊥

mod : (n : ℕ) → (m : ℕ) → m ≢ zero → Fin m
mod n zero pf = ⊥-elim (pf refl)
mod n (suc m) pf = {!!}

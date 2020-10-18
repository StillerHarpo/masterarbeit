module test where

open import Data.List
open import Data.Maybe
open import Relation.Binary.PropositionalEquality

module NewCodata where
  open import Data.Nat
  record Pair (A B : Set) : Set where
    field
      fst : A
      snd : B

  record Stream (A : Set) : Set where
    coinductive
    field
      hd : A
      tl : Stream A

  open Stream

  repeat : ∀ {A : Set} → A → Stream A
  hd (repeat x) = x
  tl (repeat x) = repeat x

  nth : ∀ {A : Set} → ℕ → Stream A → A
  nth zero s = hd s
  nth (suc n) s = nth n (tl s)

  mutual
    data μColist (A : Set) : Set where
      []   : μColist A
      _∷_ : (x : A) (xs : νColist A) → μColist A
    record νColist (A : Set) : Set where
      coinductive
      field out : μColist A

  open νColist

  record νTree (A : Set) : Set where
    coinductive
    field label : A
          subs  : List (νTree A)
  open νTree

  append : {A : Set} → List A → νColist A → νColist A
  out (append []        ys) = out ys
  out (append (x ∷ xs) ys) = x ∷ append xs ys


  appendS : {A : Set} → List A → Stream A → Stream A
  hd (appendS [] s) = hd s
  hd (appendS (x ∷ xs) s) = x
  tl (appendS [] s) = tl s
  tl (appendS (x ∷ xs) s) = tl (appendS xs s)

  record Pi (A : Set) (B : A → Set) : Set where
    field _$_ : (x : A) → B x
    infixl 20 _$_

  open Pi

  _→'_ : Set → Set → Set
  A →' B = Pi A (λ _ → B)

  infixr 20 _→'_

  plus2 : ℕ →' ℕ
  plus2 $ x = suc (suc x)

  plus : ℕ →' ℕ →' ℕ
  plus $ 0       $ m = m
  plus $ (suc n) $ m = suc (plus $ n $ m)

  data Vec (A : Set) : ℕ → Set where
    nil : Vec A 0
    _:::_ : {n : ℕ} → A → Vec A n → Vec A (suc n)

  test : Vec ℕ 2
  test = 2 ::: (2 ::: nil)

  repeat2V : Pi ℕ (λ n → Vec ℕ n)
  repeat2V $ 0     = nil
  repeat2V $ suc n =  2 ::: (repeat2V $ n)


  {-
  bf : {A : Set} → List (νTree A) → νColist A
  out (bf [])        = []
  out (bf (t ∷ ts)) = label t ∷
                       append (map label ts)
                              (bf (concatMap subs (t ∷ ts)))
  -}

  record ℕ∞ : Set where
    coinductive
    field
      pred∞ : Maybe ℕ∞
  open ℕ∞

  infinity : ℕ∞
  pred∞ infinity = just infinity

  zero∞ : ℕ∞
  pred∞ zero∞ = nothing

  one∞ : ℕ∞
  pred∞ one∞ = just zero∞

  suc∞ : ℕ∞ → ℕ∞
  pred∞ (suc∞ x) = just x

  plus∞ : ℕ∞ → ℕ∞ → ℕ∞
  pred∞ (plus∞ n m) with pred∞ n
  ...               | nothing = pred∞ m
  ...               | just n' = just (plus∞ n' m)

  {-
  record PStr (A : Set) (n : ℕ∞) : Set where
    coinductive
    field
      phd : (k : ℕ∞) → PStr A (suc∞ k) → A
      ptl : (k : ℕ∞) → PStr A (suc∞ k) → PStr A k
  -}

module OldCodata where
  open import Codata.Musical.Notation
  open import Data.Nat

  data Stream (A : Set) : Set where
     cons : A → ∞ (Stream A) → Stream A

  repeat : {A : Set} → A → Stream A
  repeat x = cons x (♯ (repeat x))

  nth : {A : Set} → ℕ → Stream A → A
  nth 0       (cons x _)  = x
  nth (suc n) (cons _ xs) = nth n (♭ xs)

  data ℕ∞ : Set where
    zero∞ : ℕ∞
    suc∞  : ∞ (ℕ∞) → ℕ∞

  infinity : ℕ∞
  infinity = suc∞ (♯ infinity)

module NonTerminating where
  open import Data.Nat

  {-
  _/_ :  ℕ → ℕ → ℕ
  zero / y = zero
  suc x / y = suc ( (x ∸ y) / y)
  -}

module SizedTypes where
  open import Agda.Builtin.Size

  data ℕ (i : Size) : Set where
    zero : ℕ i
    suc : ∀{j : Size< i} → ℕ j → ℕ i

  one : ℕ ∞
  one = suc zero
  two : ℕ ∞
  two = suc one
  three : ℕ ∞
  three = suc two
  four : ℕ ∞
  four = suc three
  five : ℕ ∞
  five = suc four

  _-_ : {i : Size} → ℕ i → ℕ ∞ → ℕ i
  zero    - _      = zero
  n       - zero   = n
  (suc n) - (suc m) = n - m

  -test₁ : five - two ≡ three
  -test₁ = refl

  _/_ : {i : Size} → ℕ i → ℕ ∞ → ℕ i
  zero  / _ = zero
  suc x / y = suc ( (x - y) / y)

  test₁ : zero / zero ≡ zero
  test₁ = refl
  test₂ : zero / five ≡ zero
  test₂ = refl
  test₃ : one / one ≡ one
  test₃ = refl
  test₄ : two / two ≡ one
  test₄ = refl
  test₅ : five / five ≡ one
  test₅ = refl
  test₆ : one / zero ≡ one
  test₆ = refl
  test₇ : two / one ≡ one
  test₇ = refl
  test₈ : four / two ≡ two
  test₈ = refl

Inductive Vec (A : Set) : nat -> Set :=
  | Nil : Vec A 0
  | Cons : forall {k : nat}, A -> Vec A k -> Vec A (S k).

Definition hd {A : Set} {k : nat} (v : Vec A (S k)) : A :=
   match v with
   | Cons _ x _ => x
   end.

Inductive nat : Set :=
| O : nat
| S : nat -> nat.

Fixpoint add (n m:nat) : nat :=
  match n with
  | O => m
  | S p => S (add p m)
  end.

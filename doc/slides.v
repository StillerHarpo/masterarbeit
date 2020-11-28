Inductive bool : Set :=
| True : bool
| False : bool.

Inductive nat : Set :=
| O : nat
| S : nat -> nat.

Definition neg (b : bool) :=
  match b with
  | True => False
  | False => True
  end.

Definition isZero (n : nat) :=
  match n with
  | O => True
  | S _ => False
  end.

CoInductive Stream (A : Set) : Set :=
  Cons : A -> Stream A -> Stream A.

CoFixpoint repeat (A : Set) (x : A) : Stream A :=
  Cons A x (repeat A x).

CoInductive U : Set := In : U -> U.

CoFixpoint u : U := In u.

Definition force (x: U) : U :=
  match x with
    In y => In y
  end.
Compute u.
Compute (force u).

Definition eq (x : U) : x = force x :=
  match x with
    In y => eq_refl
  end.

Definition eq_u : u = In u := eq u.

CoInductive Stream' (A : Set) : Set :=
  Seq { hd : A; tl : Stream' A }.

CoFixpoint repeat' (A : Set) (x : A) : Stream' A :=
  {| hd := x; tl := repeat' A x|}.

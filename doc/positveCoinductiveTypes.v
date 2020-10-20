CoInductive Stream (A:Set): Set :=
  Cons : A -> Stream A -> Stream A.

CoFixpoint repeat (A:Set) (x:A) : Stream A := Cons A x (repeat A x).

Fixpoint nth (A:Set) (n:nat) (s:Stream A) {struct n} : A :=
       match s with
         Cons _ a s =>
         match n with O => a | S p => nth A p s end
       end.

Definition tl A (s : Stream A) : Stream A :=
  match s with
  | Cons _ _ s' => s'
  end.

CoInductive U : Set := In : U -> U.

CoFixpoint u : U := In u.

Definition force (x: U) : U :=
  match x with
    In y => In y
  end.

Definition eq (x : U) : x = force x :=
  match x with
    In y => eq_refl
  end.

Definition eq_u : u = In u := eq u.

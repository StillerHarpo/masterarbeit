Inductive Vec (A : Set) : nat -> Set :=
  | Nil : Vec A 0
  | Cons : forall {k : nat}, A -> Vec A k -> Vec A (S k).

CoInductive Pi (A : Set) (B : A -> Set) := { Apply (x : A) : B x }.

CoFixpoint append2Units : Pi nat (fun n => Pi (Vec unit n)
                               (fun _ => Vec unit (S (S n)))) :=
  {| Apply n := {| Apply v := Cons _ tt (Cons _ tt v) |} |}.


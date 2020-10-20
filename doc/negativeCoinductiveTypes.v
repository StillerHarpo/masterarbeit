CoInductive Stream (A : Set) : Set :=
  Seq { hd : A; tl : Stream A }.

CoFixpoint repeat (A:Set) (x:A) : Stream A :=
  {| hd := x; tl := repeat A x|}.

Fixpoint nth (A : Set) (n : nat) (s : Stream A) : list A :=
  match n with
  | 0 => nil
  | S n' => s.(hd A) :: nth A n' s.(tl A)
  end.

CoInductive U := { out : U }.

Definition In (y : U) : U := {| out := y |}.

CoFixpoint u : U := {| out := u |}.

CoInductive Pi (A : Set) (B : A -> Set) := { Apply (x : A) : B x }.

CoFixpoint plus2 : Pi nat (fun _ => nat) :=
 {| Apply x  := S (S x) |}.

CoInductive Pi (A : Set) (B : A -> Set) := { Apply (x : A) : B x }.
CoFixpoint append2Units : Pi nat (fun n => Pi (Vec unit n)
                               (fun _ => Vec unit (S (S n)))) :=
  {| Apply n := {| Apply v := Cons _ tt (Cons _ tt v) |} |}.

Require Import Coq.Program.Program.

Set Implicit Arguments.

Notation idmap := (fun x => x).

Class MonadOps (M : Type -> Type)
  := { bind : forall {A B}, M A -> (A -> M B) -> M B;
       ret : forall {A}, A -> M A }.

Global Arguments bind {M _ A B} _ _.
Global Arguments ret {M _ A} _.

Notation "x <- y ; f" := (bind y (fun x => f)) (at level 65, right associativity).

Class MonadLaws M `{MonadOps M}
  := { bind_bind : forall {A B C} (f : A -> M B) (g : B -> M C) x,
                     bind (bind x f) g = bind x (fun u => bind (f u) g);
       bind_unit : forall {A B} (f : A -> M B) x, bind (ret x) f = f x;
       unit_bind : forall {A} (x : M A), bind x ret = x;
       bind_ext : forall {A B} {f g : A -> M B},
                    (forall x, f x = g x)
                    -> forall x, bind x f = bind x g }.

Arguments MonadLaws M {_}.

Class MonadTransformerOps (T : (Type -> Type) -> (Type -> Type))
  := { Tops :> forall M `{MonadOps M}, MonadOps (T M);
       lift : forall {M} `{MonadOps M} {A}, M A -> T M A }.

Class MonadTransformerLaws T `{MonadTransformerOps T}
  := { Tlaws :> forall {M} `{MonadLaws M}, MonadLaws (T M);
       lift_ret : forall {M} `{MonadLaws M} {A} (x : A), lift (ret x) = ret x;
       lift_bind : forall {M} `{MonadLaws M} {A B} (f : A -> M B) x,
                     lift (bind x f) = bind (lift x) (fun u => lift (f u)) }.
Arguments MonadTransformerLaws T {_}.

Create HintDb monad discriminated.
Create HintDb monad' discriminated.
Hint Rewrite @bind_bind @bind_unit @unit_bind @lift_ret @lift_bind : monad.
Hint Rewrite @bind_unit @unit_bind @lift_ret @lift_bind : monad'.
Hint Rewrite <- @bind_bind : monad'.

Instance idM : MonadOps idmap
  := { bind A B x f := f x;
       ret A x := x }.
Instance idML : MonadLaws idmap.
Proof.
  constructor; trivial.
Defined.

Instance idT : MonadTransformerOps idmap
  := { Tops M H := H;
       lift M H A x := x }.
Instance idTL : MonadTransformerLaws idmap.
Proof.
  constructor; trivial.
Defined.

Instance monad_of_transformer T `{MonadTransformerOps T}
: MonadOps (T idmap) | 1000
  := _.
Coercion monad_of_transformer : MonadTransformerOps >-> MonadOps.

Instance monad_laws_of_transformer T `{MonadTransformerLaws T}
: MonadLaws (T idmap)
  := _.
Coercion monad_laws_of_transformer : MonadTransformerLaws >-> MonadLaws.

Definition optionT :=
  {|
    Tops :=
      (fun (M : Type -> Type) (H : MonadOps M) =>
         {|
           bind :=
             (fun (A B : Type) (x : M (option A))
                (f : A -> (M (option B))) =>
                a <- x; match a return (M (option B)) with
                        | Some a' => f a'
                        | None => ret None
                        end
             )
           ;
           ret := (fun (A : Type) (x : A) => @ret M H (option A) (Some x))
         |}
      )
    ;
    lift := (fun (M : Type -> Type) (H : MonadOps M) (A : Type) (x : M A) =>
               a <- x; ret (Some a))
  |}.
(* 
Check optionT.
optionT
     : MonadTransformerOps (fun (M : Type -> Type) (A : Type) => M (option A))
 *)

Existing Instance optionT | 2.

(*
Instance optionT : MonadTransformerOps (fun M => M ∘ option) | 2
  := { Tops M H := {| ret A x := ret (Some x);
                      bind A B x f := _ |};
       lift M H A x := bind x (fun a => ret (Some a)) }.
Proof.
  { exact (bind (M:=M) x (fun a => match a with
                                   | None => ret None
                                   | Some a' => f a'
                                   end)). }
Defined.
 *)

Local Ltac t_option :=
  repeat match goal with
           | _ => intro
           | _ => reflexivity
           | _ => assumption
           | _ => solve [ trivial ]
           | [ H : option _ |- _ ] => destruct H
           | [ |- context[match ?a with None => _ end] ] => case_eq a
           | [ |- @bind ?M ?H ?A ?B ?x _ = bind ?x _ ] => let lem := constr:(@bind_ext M H _ A B) in apply lem
           | [ |- @bind ?M ?H ?A ?B ?x _ = ?x ] => etransitivity; [ | solve [ apply (@unit_bind M H _) ] ]
           | _ => progress autorewrite with monad; try assumption; []
         end.

(*
Instance optionTLaws : MonadTransformerLaws (fun M => M ∘ option) | 2
  := { Tlaws M H L := _ }.
Proof.
  { constructor; simpl; t_option. }
  { simpl; t_option. }
  { simpl; t_option. }
Defined.
 *)

(*
Definition optionTLaws'' := optionTLaws = 
{|
Tlaws := fun (M : Type -> Type) (H : MonadOps M) (L : MonadLaws M) =>
         {|
         bind_bind := fun (A B C : Type) (f : A -> (M ∘ option) B) (g : B -> (M ∘ option) C)
                        (x : (M ∘ option) A) =>
                      eq_ind_r
                        (fun m : M (option C) =>
                         m =
                         a <- x;
                         match a with
                         | Some a' => a0 <- f a'; match a0 with
                                                  | Some a'0 => g a'0
                                                  | None => ret None
                                                  end
                         | None => ret None
                         end)
                        (bind_ext
                           (fun x0 : option A =>
                            match
                              x0 as o
                              return
                                (a <- match o with
                                      | Some a' => f a'
                                      | None => ret None
                                      end; match a with
                                           | Some a' => g a'
                                           | None => ret None
                                           end =
                                 match o with
                                 | Some a' =>
                                     a <- f a'; match a with
                                                | Some a'0 => g a'0
                                                | None => ret None
                                                end
                                 | None => ret None
                                 end)
                            with
                            | Some a => eq_refl
                            | None =>
                                eq_ind_r (fun m : M (option C) => m = ret None) eq_refl
                                  (bind_unit
                                     (fun a : option B =>
                                      match a with
                                      | Some a' => g a'
                                      | None => ret None
                                      end) None)
                            end) x)
                        (bind_bind
                           (fun a : option A => match a with
                                                | Some a' => f a'
                                                | None => ret None
                                                end)
                           (fun a : option B => match a with
                                                | Some a' => g a'
                                                | None => ret None
                                                end) x);
         bind_unit := fun (A B : Type) (f : A -> (M ∘ option) B) (x : A) =>
                      eq_ind_r (fun m : M (option B) => m = f x) eq_refl
                        (bind_unit
                           (fun a : option A => match a with
                                                | Some a' => f a'
                                                | None => ret None
                                                end) (Some x));
         unit_bind := fun (A : Type) (x : (M ∘ option) A) =>
                      eq_trans
                        (bind_ext
                           (fun x0 : option A =>
                            match
                              x0 as o
                              return (match o with
                                      | Some a' => ret (Some a')
                                      | None => ret None
                                      end = ret o)
                            with
                            | Some a => eq_refl
                            | None => eq_refl
                            end) x) (unit_bind x);
         bind_ext := fun (A B : Type) (f g : A -> (M ∘ option) B) (H0 : forall x : A, f x = g x)
                       (x : (M ∘ option) A) =>
                     bind_ext
                       (fun x0 : option A =>
                        match
                          x0 as o
                          return
                            (match o with
                             | Some a' => f a'
                             | None => ret None
                             end = match o with
                                   | Some a' => g a'
                                   | None => ret None
                                   end)
                        with
                        | Some a => H0 a
                        | None => eq_refl
                        end) x |};
lift_ret := fun (M : Type -> Type) (H0 : MonadOps M) (H1 : MonadLaws M) (A : Type) (x : A) =>
            eq_ind_r (fun m : M (option A) => m = ret (Some x)) eq_refl
              (bind_unit (fun a : A => ret (Some a)) x);
lift_bind := fun (M : Type -> Type) (H0 : MonadOps M) (H1 : MonadLaws M) (A B : Type) 
               (f : A -> M B) (x : M A) =>
             eq_ind_r
               (fun m : M (option B) =>
                m =
                a <- a <- x; ret (Some a);
                match a with
                | Some a' => a0 <- f a'; ret (Some a0)
                | None => ret None
                end)
               (eq_ind_r (fun m : M (option B) => u <- x; a <- f u; ret (Some a) = m)
                  (bind_ext
                     (fun x0 : A =>
                      eq_ind_r (fun m : M (option B) => a <- f x0; ret (Some a) = m) eq_refl
                        (bind_unit
                           (fun a : option A =>
                            match a with
                            | Some a' => a0 <- f a'; ret (Some a0)
                            | None => ret None
                            end) (Some x0))) x)
                  (bind_bind (fun a : A => ret (Some a))
                     (fun a : option A =>
                      match a with
                      | Some a' => a0 <- f a'; ret (Some a0)
                      | None => ret None
                      end) x)) (bind_bind f (fun a : B => ret (Some a)) x) |}.

 *)

Definition optionTLaws :=
  {|
    Tlaws := 
      (fun (M : Type -> Type) (H : MonadOps M) (L : MonadLaws M) =>
         (*
         @Build_MonadLaws (M ∘ option)
                          (@Tops (fun M0 : Type -> Type => (M0 ∘ option)) optionT M H)
          *)
         {|
           bind_bind :=
             (fun (A B C : Type)
                   (f : A -> (M ∘ option) B)
                   (g : B -> (M ∘ option) C)
                   (x : (M ∘ option) A) =>
                 @eq_ind_r (M (option C))
                           (@bind M H (option A) (option C) x
                                  (fun u : option A =>
                                     @bind M H (option B) (option C)
                                           match u return (M (option B)) with
                                           | Some a' => f a'
                                           | None => @ret M H (option B) (@None B)
                                           end
                                           (fun a : option B =>
                                              match a return (M (option C)) with
                                              | Some a' => g a'
                                              | None => @ret M H (option C) (@None C)
                                              end)))
                           (fun m : M (option C) =>
                              @eq (@compose Type Type Type M option C) m
                                  (@bind M H (option A) (option C) x
                                         (fun a : option A =>
                                            match a return (M (option C)) with
                                            | Some a' =>
                                              @bind M H (option B) (option C) (f a')
                                                    (fun a0 : option B =>
                                                       match a0 return (M (option C)) with
                                                       | Some a'0 => g a'0
                                                       | None => @ret M H (option C) (@None C)
                                                       end)
                                            | None => @ret M H (option C) (@None C)
                                            end)))
                           (@bind_ext M H L (option A) (option C)
                                      (fun u : option A =>
                                         @bind M H (option B) (option C)
                                               match u return (M (option B)) with
                                               | Some a' => f a'
                                               | None => @ret M H (option B) (@None B)
                                               end
                                               (fun a : option B =>
                                                  match a return (M (option C)) with
                                                  | Some a' => g a'
                                                  | None => @ret M H (option C) (@None C)
                                                  end))
                                      (fun a : option A =>
                                         match a return (M (option C)) with
                                         | Some a' =>
                                           @bind M H (option B) (option C) (f a')
                                                 (fun a0 : option B =>
                                                    match a0 return (M (option C)) with
                                                    | Some a'0 => g a'0
                                                    | None => @ret M H (option C) (@None C)
                                                    end)
                                         | None => @ret M H (option C) (@None C)
                                         end)
                                      (fun x0 : option A =>
                                         match
                                           x0 as o
                                           return
                                           (@eq (M (option C))
                                                (@bind M H (option B) (option C)
                                                       match o return (M (option B)) with
                                                       | Some a' => f a'
                                                       | None => @ret M H (option B) (@None B)
                                                       end
                                                       (fun a : option B =>
                                                          match a return (M (option C)) with
                                                          | Some a' => g a'
                                                          | None => @ret M H (option C) (@None C)
                                                          end))
                                                match o return (M (option C)) with
                                                | Some a' =>
                                                  @bind M H (option B) (option C) (f a')
                                                        (fun a : option B =>
                                                           match a return (M (option C)) with
                                                           | Some a'0 => g a'0
                                                           | None => @ret M H (option C) (@None C)
                                                           end)
                                                | None => @ret M H (option C) (@None C)
                                                end)
                                         with
                                         | Some a =>
                                           @eq_refl (M (option C))
                                                    (@bind M H (option B) (option C) (f a)
                                                           (fun a0 : option B =>
                                                              match a0 return (M (option C)) with
                                                              | Some a' => g a'
                                                              | None => @ret M H (option C) (@None C)
                                                              end))
                                         | None =>
                                           @eq_ind_r (M (option C)) (@ret M H (option C) (@None C))
                                                     (fun m : M (option C) => @eq (M (option C)) m (@ret M H (option C) (@None C)))
                                                     (@eq_refl (M (option C)) (@ret M H (option C) (@None C)))
                                                     (@bind M H (option B) (option C) (@ret M H (option B) (@None B))
                                                            (fun a : option B =>
                                                               match a return (M (option C)) with
                                                               | Some a' => g a'
                                                               | None => @ret M H (option C) (@None C)
                                                               end))
                                                     (@bind_unit M H L (option B) (option C)
                                                                 (fun a : option B =>
                                                                    match a return (M (option C)) with
                                                                    | Some a' => g a'
                                                                    | None => @ret M H (option C) (@None C)
                                                                    end) (@None B))
                                         end) x)
                           (@bind M H (option B) (option C)
                                  (@bind M H (option A) (option B) x
                                         (fun a : option A =>
                                            match a return (M (option B)) with
                                            | Some a' => f a'
                                            | None => @ret M H (option B) (@None B)
                                            end))
                                  (fun a : option B =>
                                     match a return (M (option C)) with
                                     | Some a' => g a'
                                     | None => @ret M H (option C) (@None C)
                                     end))
                           (@bind_bind M H L (option A) (option B) (option C)
                                       (fun a : option A =>
                                          match a return (M (option B)) with
                                          | Some a' => f a'
                                          | None => @ret M H (option B) (@None B)
                                          end)
                                       (fun a : option B =>
                                          match a return (M (option C)) with
                                          | Some a' => g a'
                                          | None => @ret M H (option C) (@None C)
                                          end) x))
              :
                forall (A B C : Type) (f : A -> (M ∘ option) B)
                       (g : B -> (M ∘ option) C) (x : (M ∘ option) A),
                  @eq ((M ∘ option) C)
                      (@bind (M ∘ option)
                             (@Tops (fun M0 : forall _ : Type, Type => @compose Type Type Type M0 option) optionT M H) B C
                             (@bind (@compose Type Type Type M option)
                                    (@Tops (fun M0 : forall _ : Type, Type => @compose Type Type Type M0 option) optionT M H) A B x
                                    f) g)
                      (@bind (M ∘ option)
                             (@Tops (fun M0 : forall _ : Type, Type => @compose Type Type Type M0 option) optionT M H) A C x
                             (fun u : A =>
                                @bind (@compose Type Type Type M option)
                                      (@Tops (fun M0 : forall _ : Type, Type => @compose Type Type Type M0 option) optionT M H) B C
                                      (f u) g))
           ;
           bind_unit :=
                          ((fun (A B : Type) (f : forall _ : A, @compose Type Type Type M option B) (x : A) =>
                              @eq_ind_r (M (option B)) (f x)
                                        (fun m : M (option B) => @eq (@compose Type Type Type M option B) m (f x))
                                        (@eq_refl (@compose Type Type Type M option B) (f x))
                                        (@bind M H (option A) (option B) (@ret M H (option A) (@Some A x))
                                               (fun a : option A =>
                                                  match a return (M (option B)) with
                                                  | Some a' => f a'
                                                  | None => @ret M H (option B) (@None B)
                                                  end))
                                        (@bind_unit M H L (option A) (option B)
                                                    (fun a : option A =>
                                                       match a return (M (option B)) with
                                                       | Some a' => f a'
                                                       | None => @ret M H (option B) (@None B)
                                                       end) (@Some A x)))
                          )
           ;
           unit_bind :=
                          ((fun (A : Type) (x : @compose Type Type Type M option A) =>
                              @eq_trans (@compose Type Type Type M option A)
                                        (@bind M H (option A) (option A) x
                                               (fun a : option A =>
                                                  match a return (M (option A)) with
                                                  | Some a' => @ret M H (option A) (@Some A a')
                                                  | None => @ret M H (option A) (@None A)
                                                  end)) (@bind M H (option A) (option A) x (@ret M H (option A))) x
                                        (@bind_ext M H L (option A) (option A)
                                                   (fun a : option A =>
                                                      match a return (M (option A)) with
                                                      | Some a' => @ret M H (option A) (@Some A a')
                                                      | None => @ret M H (option A) (@None A)
                                                      end) (@ret M H (option A))
                                                   (fun x0 : option A =>
                                                      match
                                                        x0 as o
                                                        return
                                                        (@eq (M (option A))
                                                             match o return (M (option A)) with
                                                             | Some a' => @ret M H (option A) (@Some A a')
                                                             | None => @ret M H (option A) (@None A)
                                                             end (@ret M H (option A) o))
                                                      with
                                                      | Some a => @eq_refl (M (option A)) (@ret M H (option A) (@Some A a))
                                                      | None => @eq_refl (M (option A)) (@ret M H (option A) (@None A))
                                                      end) x) (@unit_bind M H L (option A) x))
                           :
                             forall (A : Type) (x : @compose Type Type Type M option A),
                               @eq (@compose Type Type Type M option A)
                                   (@bind (@compose Type Type Type M option)
                                          (@Tops (fun M0 : forall _ : Type, Type => @compose Type Type Type M0 option) optionT M H) A A x
                                          (@ret (@compose Type Type Type M option)
                                                (@Tops (fun M0 : forall _ : Type, Type => @compose Type Type Type M0 option) optionT M H) A)) x)
           ;
           bind_ext :=
                          ((fun (A B : Type) (f g : forall _ : A, @compose Type Type Type M option B)
                                (H0 : forall x : A, @eq (@compose Type Type Type M option B) (f x) (g x))
                                (x : @compose Type Type Type M option A) =>
                              @bind_ext M H L (option A) (option B)
                                        (fun a : option A =>
                                           match a return (M (option B)) with
                                           | Some a' => f a'
                                           | None => @ret M H (option B) (@None B)
                                           end)
                                        (fun a : option A =>
                                           match a return (M (option B)) with
                                           | Some a' => g a'
                                           | None => @ret M H (option B) (@None B)
                                           end)
                                        (fun x0 : option A =>
                                           match
                                             x0 as o
                                             return
                                             (@eq (M (option B))
                                                  match o return (M (option B)) with
                                                  | Some a' => f a'
                                                  | None => @ret M H (option B) (@None B)
                                                  end
                                                  match o return (M (option B)) with
                                                  | Some a' => g a'
                                                  | None => @ret M H (option B) (@None B)
                                                  end)
                                           with
                                           | Some a => H0 a
                                           | None => @eq_refl (M (option B)) (@ret M H (option B) (@None B))
                                           end) x)
                           :
                             forall (A B : Type) (f g : forall _ : A, @compose Type Type Type M option B)
                                    (_ : forall x : A, @eq (@compose Type Type Type M option B) (f x) (g x))
                                    (x : @compose Type Type Type M option A),
                               @eq (@compose Type Type Type M option B)
                                   (@bind (@compose Type Type Type M option)
                                          (@Tops (fun M0 : forall _ : Type, Type => @compose Type Type Type M0 option) optionT M H) A B x f)
                                   (@bind (@compose Type Type Type M option)
                                          (@Tops (fun M0 : forall _ : Type, Type => @compose Type Type Type M0 option) optionT M H) A B x g))
         |}
           
      )
    ;
    lift_ret :=
      (((fun (M : forall _ : Type, Type) (H0 : MonadOps M) (H1 : @MonadLaws M H0) (A : Type) (x : A) =>
           @eq_ind_r (M (option A)) (@ret M H0 (option A) (@Some A x))
                     (fun m : M (option A) => @eq (@compose Type Type Type M option A) m (@ret M H0 (option A) (@Some A x)))
                     (@eq_refl (@compose Type Type Type M option A) (@ret M H0 (option A) (@Some A x)))
                     (@bind M H0 A (option A) (@ret M H0 A x) (fun a : A => @ret M H0 (option A) (@Some A a)))
                     (@bind_unit M H0 H1 A (option A) (fun a : A => @ret M H0 (option A) (@Some A a)) x))
        :
          forall (M : forall _ : Type, Type) (H0 : MonadOps M) (_ : @MonadLaws M H0) (A : Type) (x : A),
            @eq (@compose Type Type Type M option A)
                (@lift (fun M0 : forall _ : Type, Type => @compose Type Type Type M0 option) optionT M H0 A
                       (@ret M H0 A x))
                (@ret (@compose Type Type Type M option)
                      (@Tops (fun M0 : forall _ : Type, Type => @compose Type Type Type M0 option) optionT M H0) A x))
      )
    ;
    lift_bind :=
      (((fun (M : forall _ : Type, Type) (H0 : MonadOps M) (H1 : @MonadLaws M H0) (A B : Type)
             (f : forall _ : A, M B) (x : M A) =>
           @eq_ind_r (M (option B))
                     (@bind M H0 A (option B) x
                            (fun u : A => @bind M H0 B (option B) (f u) (fun a : B => @ret M H0 (option B) (@Some B a))))
                     (fun m : M (option B) =>
                        @eq (@compose Type Type Type M option B) m
                            (@bind M H0 (option A) (option B)
                                   (@bind M H0 A (option A) x (fun a : A => @ret M H0 (option A) (@Some A a)))
                                   (fun a : option A =>
                                      match a return (M (option B)) with
                                      | Some a' => @bind M H0 B (option B) (f a') (fun a0 : B => @ret M H0 (option B) (@Some B a0))
                                      | None => @ret M H0 (option B) (@None B)
                                      end)))
                     (@eq_ind_r (M (option B))
                                (@bind M H0 A (option B) x
                                       (fun u : A =>
                                          @bind M H0 (option A) (option B) (@ret M H0 (option A) (@Some A u))
                                                (fun a : option A =>
                                                   match a return (M (option B)) with
                                                   | Some a' =>
                                                     @bind M H0 B (option B) (f a') (fun a0 : B => @ret M H0 (option B) (@Some B a0))
                                                   | None => @ret M H0 (option B) (@None B)
                                                   end)))
                                (fun m : M (option B) =>
                                   @eq (@compose Type Type Type M option B)
                                       (@bind M H0 A (option B) x
                                              (fun u : A => @bind M H0 B (option B) (f u) (fun a : B => @ret M H0 (option B) (@Some B a))))
                                       m)
                                (@bind_ext M H0 H1 A (option B)
                                           (fun u : A => @bind M H0 B (option B) (f u) (fun a : B => @ret M H0 (option B) (@Some B a)))
                                           (fun u : A =>
                                              @bind M H0 (option A) (option B) (@ret M H0 (option A) (@Some A u))
                                                    (fun a : option A =>
                                                       match a return (M (option B)) with
                                                       | Some a' =>
                                                         @bind M H0 B (option B) (f a') (fun a0 : B => @ret M H0 (option B) (@Some B a0))
                                                       | None => @ret M H0 (option B) (@None B)
                                                       end))
                                           (fun x0 : A =>
                                              @eq_ind_r (M (option B))
                                                        (@bind M H0 B (option B) (f x0) (fun a : B => @ret M H0 (option B) (@Some B a)))
                                                        (fun m : M (option B) =>
                                                           @eq (M (option B))
                                                               (@bind M H0 B (option B) (f x0) (fun a : B => @ret M H0 (option B) (@Some B a))) m)
                                                        (@eq_refl (M (option B))
                                                                  (@bind M H0 B (option B) (f x0) (fun a : B => @ret M H0 (option B) (@Some B a))))
                                                        (@bind M H0 (option A) (option B) (@ret M H0 (option A) (@Some A x0))
                                                               (fun a : option A =>
                                                                  match a return (M (option B)) with
                                                                  | Some a' =>
                                                                    @bind M H0 B (option B) (f a') (fun a0 : B => @ret M H0 (option B) (@Some B a0))
                                                                  | None => @ret M H0 (option B) (@None B)
                                                                  end))
                                                        (@bind_unit M H0 H1 (option A) (option B)
                                                                    (fun a : option A =>
                                                                       match a return (M (option B)) with
                                                                       | Some a' =>
                                                                         @bind M H0 B (option B) (f a') (fun a0 : B => @ret M H0 (option B) (@Some B a0))
                                                                       | None => @ret M H0 (option B) (@None B)
                                                                       end) (@Some A x0))) x)
                                (@bind M H0 (option A) (option B)
                                       (@bind M H0 A (option A) x (fun a : A => @ret M H0 (option A) (@Some A a)))
                                       (fun a : option A =>
                                          match a return (M (option B)) with
                                          | Some a' => @bind M H0 B (option B) (f a') (fun a0 : B => @ret M H0 (option B) (@Some B a0))
                                          | None => @ret M H0 (option B) (@None B)
                                          end))
                                (@bind_bind M H0 H1 A (option A) (option B) (fun a : A => @ret M H0 (option A) (@Some A a))
                                            (fun a : option A =>
                                               match a return (M (option B)) with
                                               | Some a' => @bind M H0 B (option B) (f a') (fun a0 : B => @ret M H0 (option B) (@Some B a0))
                                               | None => @ret M H0 (option B) (@None B)
                                               end) x))
                     (@bind M H0 B (option B) (@bind M H0 A B x f) (fun a : B => @ret M H0 (option B) (@Some B a)))
                     (@bind_bind M H0 H1 A B (option B) f (fun a : B => @ret M H0 (option B) (@Some B a)) x))
        :
          forall (M : forall _ : Type, Type) (H0 : MonadOps M) (_ : @MonadLaws M H0) (A B : Type)
                 (f : forall _ : A, M B) (x : M A),
            @eq (@compose Type Type Type M option B)
                (@lift (fun M0 : forall _ : Type, Type => @compose Type Type Type M0 option) optionT M H0 B
                       (@bind M H0 A B x f))
                (@bind (@compose Type Type Type M option)
                       (@Tops (fun M0 : forall _ : Type, Type => @compose Type Type Type M0 option) optionT M H0) A B
                       (@lift (fun M0 : forall _ : Type, Type => @compose Type Type Type M0 option) optionT M H0 A x)
                       (fun u : A =>
                          @lift (fun M0 : forall _ : Type, Type => @compose Type Type Type M0 option) optionT M H0 B (f u))))
      )
  |}
  .
(*
  Check optionTLaws.

  optionTLaws
       : MonadTransformerLaws (fun M : Type -> Type => M ∘ option)
*)

Existing Instance optionTLaws | 2.

Instance optionM : MonadOps option | 2 := optionT.
Instance optionLaws : MonadLaws option | 2 := optionTLaws.

Definition liftA {T} `{MonadTransformerOps T} {M} `{MonadOps M} {A B}
           (f : M A -> M B)
: T M A -> T M B
  := fun x => y <- x; lift (f (ret y)).

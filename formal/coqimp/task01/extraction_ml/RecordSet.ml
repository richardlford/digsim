
type ('e, 't) coq_Reader = 'e -> 't

(** val constructor : 'a2 -> ('a1, 'a2) coq_Reader **)

let constructor x _ =
  x

type ('r, 't) coq_Setter = ('t -> 't) -> 'r -> 'r
  (* singleton inductive, whose constructor was Build_Setter *)

(** val set : ('a1 -> 'a2) -> ('a1, 'a2) coq_Setter -> ('a2 -> 'a2) -> 'a1 -> 'a1 **)

let set _ setter =
  setter

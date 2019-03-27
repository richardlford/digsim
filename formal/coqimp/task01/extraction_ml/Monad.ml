
type __ = Obj.t
let __ = let rec f _ = Obj.repr f in Obj.repr f

type 'm coq_MonadOps = { bind : (__ -> __ -> 'm -> (__ -> 'm) -> 'm); ret : (__ -> __ -> 'm) }

(** val bind : 'a1 coq_MonadOps -> 'a1 -> ('a2 -> 'a1) -> 'a1 **)

let bind monadOps x x0 =
  let { bind = bind0; ret = _ } = monadOps in Obj.magic bind0 __ __ x x0

(** val ret : 'a1 coq_MonadOps -> 'a2 -> 'a1 **)

let ret monadOps x =
  let { bind = _; ret = ret0 } = monadOps in Obj.magic ret0 __ x

type 't coq_MonadTransformerOps = { coq_Tops : (__ -> __ coq_MonadOps -> 't coq_MonadOps);
                                    lift : (__ -> __ coq_MonadOps -> __ -> __ -> 't) }

(** val coq_Tops : 'a1 coq_MonadTransformerOps -> 'a2 coq_MonadOps -> 'a1 coq_MonadOps **)

let coq_Tops monadTransformerOps h =
  let { coq_Tops = tops; lift = _ } = monadTransformerOps in Obj.magic tops __ h

(** val idM : __ coq_MonadOps **)

let idM =
  { bind = (fun _ _ x f -> f x); ret = (fun _ x -> x) }

(** val monad_of_transformer : 'a1 coq_MonadTransformerOps -> 'a1 coq_MonadOps **)

let monad_of_transformer h =
  coq_Tops h idM

(** val optionT : __ coq_MonadTransformerOps **)

let optionT =
  { coq_Tops = (fun _ h -> { bind = (fun _ _ x f ->
    bind h x (fun a -> match a with
                       | Some a' -> f a'
                       | None -> ret h None)); ret = (fun _ x -> ret h (Some x)) }); lift = (fun _ h _ x ->
    bind h x (fun a -> ret h (Some a))) }

(** val optionM : __ option coq_MonadOps **)

let optionM =
  monad_of_transformer (Obj.magic optionT)

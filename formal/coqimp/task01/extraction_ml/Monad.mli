
type __ = Obj.t

type 'm coq_MonadOps = { bind : (__ -> __ -> 'm -> (__ -> 'm) -> 'm); ret : (__ -> __ -> 'm) }

val bind : 'a1 coq_MonadOps -> 'a1 -> ('a2 -> 'a1) -> 'a1

val ret : 'a1 coq_MonadOps -> 'a2 -> 'a1

type 't coq_MonadTransformerOps = { coq_Tops : (__ -> __ coq_MonadOps -> 't coq_MonadOps);
                                    lift : (__ -> __ coq_MonadOps -> __ -> __ -> 't) }

val coq_Tops : 'a1 coq_MonadTransformerOps -> 'a2 coq_MonadOps -> 'a1 coq_MonadOps

val idM : __ coq_MonadOps

val monad_of_transformer : 'a1 coq_MonadTransformerOps -> 'a1 coq_MonadOps

val optionT : __ coq_MonadTransformerOps

val optionM : __ option coq_MonadOps

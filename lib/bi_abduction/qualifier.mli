(** Qualifiers for bi-abductive inference. *)

type t = Request.t

val owned : ct:Sctypes.t -> pointer:IndexTerms.t -> t
val predicate : name:Sym.t -> pointer:IndexTerms.t -> iargs:IndexTerms.t list -> t
val pp : t -> Pp.document
val equal : t -> t -> bool

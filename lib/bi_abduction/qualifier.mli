(** Qualifiers for bi-abductive inference. *)

type chain_step =
  { bind : Sym.t;
    resource : Request.t
  }

type chain = chain_step list
type t = Request.t

val owned : ct:Sctypes.t -> pointer:IndexTerms.t -> t
val predicate : name:Sym.t -> pointer:IndexTerms.t -> iargs:IndexTerms.t list -> t
val pp_chain : chain -> Pp.document
val pp : t -> Pp.document
val free_vars : t -> Sym.Set.t
val subst : [ `Rename of Sym.t | `Term of IndexTerms.t ] Subst.t -> t -> t

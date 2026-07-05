(** Qualifiers for bi-abductive inference: chains of named [take] bindings.
    Singleton chains are today's flat qualifiers; multi-step chains
    (IDEA.md 4.4) extend the same type. *)

type step =
  { name : Sym.t;
    req : Request.t
  }

type t = step list

val owned : ct:Sctypes.t -> pointer:IndexTerms.t -> t

val predicate : name:Sym.t -> pointer:IndexTerms.t -> iargs:IndexTerms.t list -> t

(** The request of a singleton chain, if it is one. *)
val singleton_req : t -> Request.t option

(** Render as CN [take] line(s), suitable for spec suggestions. *)
val pp_takes : t -> Pp.document

(** Compact rendering (no [take]), for debug output. *)
val pp : t -> Pp.document

(** Equality modulo bound names (exact for singleton chains). *)
val equal : t -> t -> bool

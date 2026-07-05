(** Bounded enumeration of candidate qualifiers. *)

type arg =
  { sym : Sym.t;
    bt : BaseTypes.t;
    owned_ct : Sctypes.t option
  }

type config = { max_qualifiers : int }

val default_config : config

(** Enumerate candidate qualifiers for a function: flat Owned/predicate
    qualifiers rooted at arguments, plus depth-2 chains through the
    pointer-typed fields of struct-pointer arguments (IDEA.md 4.4). *)
val enumerate
  :  config:config ->
  args:arg list ->
  pred_defs:Definition.Predicate.t Sym.Map.t ->
  struct_defs:(Id.t * Sctypes.t) list Sym.Map.t ->
  loc:Locations.t ->
  Qualifier.t list

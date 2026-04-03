(** Bounded enumeration of candidate qualifiers. *)

type config =
  { max_term_depth : int;
    max_qualifiers : int;
    max_chain_depth : int;
    max_pred_unfolding : int
  }

val default_config : config

(** Generate base pointer terms from function arguments. *)
val base_pointer_terms
  :  (Sym.t * BaseTypes.t) list ->
  Locations.t ->
  IndexTerms.t list

(** Extract access paths from a recursive predicate definition. *)
val traversal_fields
  :  Definition.Predicate.t ->
  struct_defs:(Id.t * Sctypes.t) list Sym.Map.t ->
  Id.t list list

(** Enumerate candidate qualifiers for a function. *)
val enumerate
  :  config:config ->
  args:(Sym.t * BaseTypes.t) list ->
  pred_defs:Definition.Predicate.t Sym.Map.t ->
  struct_defs:(Id.t * Sctypes.t) list Sym.Map.t ->
  graph:Memory_graph.t ->
  var_addrs:(string * int64) list ->
  loc:Locations.t ->
  Qualifier.t list

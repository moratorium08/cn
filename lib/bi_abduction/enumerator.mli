(** Bounded enumeration of candidate qualifiers. *)

type arg =
  { sym : Sym.t;
    bt : BaseTypes.t;
    owned_ct : Sctypes.t option
  }

type config = { max_qualifiers : int }

val default_config : config

(** Enumerate candidate qualifiers for a function. *)
val enumerate
  :  config:config ->
  args:arg list ->
  pred_defs:Definition.Predicate.t Sym.Map.t ->
  loc:Locations.t ->
  Qualifier.t list

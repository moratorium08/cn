(** Top-level inference orchestrator. *)

type inferred_qualifiers =
  { pre : Qualifier.t list option; (** [None] when the pre cover failed *)
    post : Qualifier.t list option (** [None] when the post cover failed *)
  }

type inferred_spec =
  { function_name : string;
    qualifiers : inferred_qualifiers
  }

val infer
  :  config:Enumerator.config ->
  harness:Footprint.harness_ctx ->
  summary_file:string ->
  heap_file:string ->
  pred_defs:Definition.Predicate.t Sym.Map.t ->
  struct_defs:(Id.t * Sctypes.t) list Sym.Map.t ->
  function_args:(string * (string * Sctypes.t) list) list ->
  inferred_spec list

val pp_suggestions : inferred_spec list -> Pp.document

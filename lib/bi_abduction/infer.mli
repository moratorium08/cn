(** Top-level inference orchestrator. *)

type inferred_qualifiers =
  { pre : Qualifier.t list;
    post : Qualifier.t list
  }

type inferred_spec =
  { function_name : string;
    qualifiers : inferred_qualifiers option (** [None] when cover failed. *)
  }

val infer
  :  config:Enumerator.config ->
  execution_data:Data_point.execution_data ->
  pred_defs:Definition.Predicate.t Sym.Map.t ->
  struct_defs:(Id.t * Sctypes.t) list Sym.Map.t ->
  function_args:(string * (string * Sctypes.t) list) list ->
  inferred_spec list

val pp_suggestions : inferred_spec list -> Pp.document

val infer_from_files
  :  config:Enumerator.config ->
  summary_file:string ->
  heap_file:string ->
  pred_defs:Definition.Predicate.t Sym.Map.t ->
  struct_defs:(Id.t * Sctypes.t) list Sym.Map.t ->
  function_args:(string * (string * Sctypes.t) list) list ->
  inferred_spec list

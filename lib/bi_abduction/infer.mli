(** Top-level inference orchestrator. *)

module CF = Cerb_frontend

type inferred_qualifiers =
  { pre : Qualifier.t list;
    post : Qualifier.t list
  }

type inferred_spec =
  { function_name : string;
    qualifiers : inferred_qualifiers option (** [None] when cover failed. *)
  }

(** Context the inference pipeline needs to drive the predicate-footprint
    harness: a C compiler, an output directory, the runtime layout, and
    the parsed program inputs from cerberus / fulminate. *)
type harness_ctx =
  { cc : string;
    output_dir : string;
    cn_runtime_prefix : string;
    filename : string;
    cabs_tunit : CF.Cabs.translation_unit;
    ail_prog : CF.GenTypes.genTypeCategory CF.AilSyntax.sigma;
    prog5 : unit Mucore.file
  }

val infer
  :  config:Enumerator.config ->
  harness:harness_ctx ->
  summary_file:string ->
  heap_file:string ->
  pred_defs:Definition.Predicate.t Sym.Map.t ->
  struct_defs:(Id.t * Sctypes.t) list Sym.Map.t ->
  function_args:(string * (string * Sctypes.t) list) list ->
  inferred_spec list

val pp_suggestions : inferred_spec list -> Pp.document

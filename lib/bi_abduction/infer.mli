(** Top-level inference orchestrator. *)

module Int64Set = Data_point.Int64Set

type address_analysis =
  { var_name : string;
    var_addr : int64;
    missing_range : int
  }

type inferred_spec =
  { function_name : string;
    pre_qualifiers : Qualifier.t list;
    post_qualifiers : Qualifier.t list;
    pre_uncovered : Int64Set.t;
    post_uncovered : Int64Set.t;
    pre_analysis : address_analysis list;
    post_analysis : address_analysis list
  }

val infer
  :  config:Enumerator.config
  -> execution_data:Data_point.execution_data
  -> heap_lookup:(int64 -> int64 option)
  -> pred_defs:Definition.Predicate.t Sym.Map.t
  -> struct_defs:(Id.t * Sctypes.t) list Sym.Map.t
  -> function_args:(string * (string * Sctypes.t) list) list
  -> inferred_spec list

val pp_suggestions : inferred_spec list -> Pp.document

val infer_from_files
  :  config:Enumerator.config
  -> summary_file:string
  -> heap_file:string
  -> pred_defs:Definition.Predicate.t Sym.Map.t
  -> struct_defs:(Id.t * Sctypes.t) list Sym.Map.t
  -> function_args:(string * (string * Sctypes.t) list) list
  -> inferred_spec list

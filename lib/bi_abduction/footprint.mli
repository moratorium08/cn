(** Footprint computation for qualifiers. *)

module CF = Cerb_frontend
module Int64Set = Data_point.Int64Set

(** Context needed to drive the predicate-footprint harness: a C compiler,
    an output directory, the runtime layout, and the parsed program inputs
    from cerberus / fulminate. *)
type harness_ctx =
  { cc : string;
    output_dir : string;
    cn_runtime_prefix : string;
    filename : string;
    cabs_tunit : CF.Cabs.translation_unit;
    ail_prog : CF.GenTypes.genTypeCategory CF.AilSyntax.sigma;
    prog5 : unit Mucore.file
  }

val owned_footprint : ct:Sctypes.t -> base_addr:int64 -> Int64Set.t

val compute : Qualifier.t -> Data_point.data_point -> Int64Set.t option

val compute_batch
  :  Qualifier.t list ->
  Data_point.data_point ->
  (Qualifier.t * Int64Set.t option) list

val compute_predicate_table
  :  harness:harness_ctx ->
  tag:string ->
  func_name:string ->
  pred_defs:Definition.Predicate.t Sym.Map.t ->
  data_points:Fp_codegen.dp_entry list ->
  qualifiers:(int * Qualifier.t) list ->
  Fp_table.t

val lookup
  :  representative_dp:Data_point.data_point ->
  representative_dp_idx:int ->
  fp_table:Fp_table.t ->
  int * Qualifier.t ->
  Int64Set.t option

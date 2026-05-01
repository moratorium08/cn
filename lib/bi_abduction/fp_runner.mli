(** Drive the bi-abductive footprint harness end to end:
    [Fp_codegen.emit] → write to disk → [cc] → run → parse JSON. *)

module CF = Cerb_frontend

(** Compile and execute the harness, returning a populated [Fp_table].

    [output_dir] should already exist; the harness source, object and
    binary files are written there.  [cn_runtime_prefix] must contain
    [include/] and [libcn_exec.a] (the same layout used by [cn bi-abd]).
    [tag] is a short suffix included in the emitted file names so callers
    can run the harness multiple times per function (e.g. one run per
    pre/post phase) without clobbering the previous artefacts. *)
val run
  :  cc:string ->
  output_dir:string ->
  cn_runtime_prefix:string ->
  func_name:string ->
  tag:string ->
  filename:string ->
  cabs_tunit:CF.Cabs.translation_unit ->
  ail_prog:CF.GenTypes.genTypeCategory CF.AilSyntax.sigma ->
  prog5:unit Mucore.file ->
  pred_defs:Definition.Predicate.t Sym.Map.t ->
  data_points:Fp_codegen.dp_entry list ->
  qualifiers:(int * Qualifier.t) list ->
  Fp_table.t

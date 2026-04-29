(** Drive the bi-abductive footprint harness end to end:
    [Fp_codegen.emit] → write to disk → [cc] → run → parse JSON. *)

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
  Fp_codegen.input ->
  Fp_table.t

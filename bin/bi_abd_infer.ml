(** CLI subcommand for bi-abductive inference.

    Reads the summary JSON and heap JSONL files produced by a bi-abductive
    execution run, runs the inference pipeline, and prints suggested
    CN specifications. *)

open Cmdliner
open Cn

let run_inference ~summary_file ~heap_file =
  let config = Bi_abduction.Enumerator.default_config in
  let specs =
    Bi_abduction.Infer.infer_from_files
      ~config
      ~summary_file
      ~heap_file
      ~pred_defs:Sym.Map.empty
      ~struct_defs:Sym.Map.empty
  in
  let doc = Bi_abduction.Infer.pp_suggestions specs in
  Pp.print stdout doc;
  Format.printf "@."

let generate_infer summary_file heap_file =
  run_inference ~summary_file ~heap_file

module Flags = struct
  let summary_file =
    let doc = "Path to the bi-abductive summary file (cn_abd_summary.json)." in
    Arg.(
      value
      & opt string "cn_abd_summary.json"
      & info [ "summary" ] ~docv:"FILE" ~doc)

  let heap_file =
    let doc = "Path to the bi-abductive heap dump file (cn_abd_heap.jsonl)." in
    Arg.(
      value
      & opt string "cn_abd_heap.jsonl"
      & info [ "heap" ] ~docv:"FILE" ~doc)
end

let cmd =
  let open Term in
  let infer_t = const generate_infer $ Flags.summary_file $ Flags.heap_file in
  let doc =
    "Analyse bi-abductive execution output and suggest CN specifications.\n\
     Reads the summary JSON and heap JSONL files produced by running an \
     instrumented binary with --bi-abd, then runs the inference pipeline."
  in
  let info = Cmd.info "infer" ~doc in
  Cmd.v info infer_t

(** CLI subcommand for bi-abductive inference.

    Reads the summary JSON and heap JSONL files produced by a bi-abductive
    execution run, parses the original C source for struct/predicate
    definitions, runs the inference pipeline, and prints suggested
    CN specifications. *)

module CF = Cerb_frontend
module C = CF.Ctype
module A = CF.AilSyntax
open Cmdliner
open Cn

(** Extract struct definitions from the Ail sigma.
    Returns a Sym.Map from struct tag to list of (field_id, field_sctypes). *)
let extract_struct_defs (sigm : _ A.sigma) : (Id.t * Sctypes.t) list Sym.Map.t =
  List.fold_left
    (fun acc (tag_sym, (_, _, tag_def)) ->
       match tag_def with
       | C.StructDef (members, _) ->
         let fields =
           List.filter_map
             (fun (field_id, (_, _, _, field_ctype)) ->
                match Sctypes.of_ctype field_ctype with
                | Some sct -> Some (field_id, sct)
                | None -> None)
             members
         in
         Sym.Map.add tag_sym fields acc
       | C.UnionDef _ -> acc)
    Sym.Map.empty
    sigm.A.tag_definitions

(** Extract predicate definitions from the mucore file. *)
let extract_pred_defs (prog5 : unit Mucore.file)
  : Definition.Predicate.t Sym.Map.t
  =
  List.fold_left
    (fun acc (sym, pred_def) -> Sym.Map.add sym pred_def acc)
    Sym.Map.empty
    prog5.resource_predicates

let run_inference_with_defs ~summary_file ~heap_file ~struct_defs ~pred_defs =
  let config = Bi_abduction.Enumerator.default_config in
  let specs =
    Bi_abduction.Infer.infer_from_files
      ~config
      ~summary_file
      ~heap_file
      ~pred_defs
      ~struct_defs
  in
  let doc = Bi_abduction.Infer.pp_suggestions specs in
  Pp.print stdout doc;
  Format.printf "@."

let generate_infer
      filename
      cc
      macros
      permissive
      incl_dirs
      incl_files
      loc_pp
      debug_level
      print_level
      print_sym_nums
      no_timestamps
      diag
      csv_times
      astprints
      dont_use_vip
      no_inherit_loc
      magic_comment_char_dollar
      allow_split_magic_comments
      summary_file
      heap_file
  =
  Cerb_debug.debug_level := debug_level;
  Pp.loc_pp := loc_pp;
  Pp.print_level := print_level;
  Sym.print_nums := print_sym_nums;
  Pp.print_timestamps := not no_timestamps;
  IndexTerms.use_vip := not dont_use_vip;
  Diagnostics.diag_string := diag;
  let handle_error (e : TypeErrors.t) =
    let report = TypeErrors.pp_message e.msg in
    Pp.error e.loc report.short (Option.to_list report.descr);
    match e.msg with TypeErrors.Unsupported _ -> exit 2 | _ -> exit 1
  in
  let filename = Common.there_can_only_be_one filename in
  Common.with_well_formedness_check
    ~filename
    ~cc
    ~macros
    ~permissive
    ~incl_dirs
    ~incl_files
    ~coq_export_file:None
    ~coq_mucore:false
    ~coq_proof_log:false
    ~coq_check_proof_log:false
    ~csv_times
    ~astprints
    ~no_inherit_loc
    ~magic_comment_char_dollar
    ~allow_split_magic_comments
    ~save_cpp:None
    ~disable_linemarkers:false
    ~skip_label_inlining:false
    ~handle_error
    ~f:(fun ~cabs_tunit:_ ~prog5 ~ail_prog ~statement_locs:_ ~paused:_ ->
      let _startup_sym, sigm = ail_prog in
      let struct_defs = extract_struct_defs sigm in
      let pred_defs = extract_pred_defs prog5 in
      run_inference_with_defs ~summary_file ~heap_file ~struct_defs ~pred_defs;
      Ok ())

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
  let infer_t =
    const generate_infer
    $ Common.Flags.file
    $ Common.Flags.cc
    $ Common.Flags.macros
    $ Common.Flags.permissive
    $ Common.Flags.incl_dirs
    $ Common.Flags.incl_files
    $ Verify.Flags.loc_pp
    $ Common.Flags.debug_level
    $ Common.Flags.print_level
    $ Common.Flags.print_sym_nums
    $ Common.Flags.no_timestamps
    $ Verify.Flags.diag
    $ Common.Flags.csv_times
    $ Common.Flags.astprints
    $ Verify.Flags.dont_use_vip
    $ Common.Flags.no_inherit_loc
    $ Common.Flags.magic_comment_char_dollar
    $ Common.Flags.allow_split_magic_comments
    $ Flags.summary_file
    $ Flags.heap_file
  in
  let doc =
    "Analyse bi-abductive execution output and suggest CN specifications.\n\
     Requires the original C source [FILE] for struct and predicate \
     definitions, plus the summary JSON and heap JSONL files from --bi-abd."
  in
  let info = Cmd.info "infer" ~doc in
  Cmd.v info infer_t

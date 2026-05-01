(** CLI subcommand for push-button bi-abductive inference.

    Single command that instruments a C file, compiles and runs it
    in bi-abductive mode, then analyses the output to suggest CN
    specifications. *)

module CF = Cerb_frontend
module C = CF.Ctype
module A = CF.AilSyntax
open Cmdliner
open Cn

(** Extract struct definitions from the Ail sigma. *)
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
let extract_pred_defs (prog5 : unit Mucore.file) : Definition.Predicate.t Sym.Map.t =
  List.fold_left
    (fun acc (sym, pred_def) -> Sym.Map.add sym pred_def acc)
    Sym.Map.empty
    prog5.resource_predicates


(** Extract function parameter types from the Ail sigma.
    Maps function name -> list of (parameter name, parameter type).
    Functions with any parameter whose C type cannot be represented as
    [Sctypes.t] are dropped entirely with a warning, so that downstream
    inference never runs on a function with partial signature info. *)
let extract_function_args (sigm : _ A.sigma) : (string * (string * Sctypes.t) list) list =
  List.filter_map
    (fun (fn_sym, (_, _, _, param_syms, _)) ->
       match List.assoc_opt Sym.equal fn_sym sigm.A.declarations with
       | Some (_, _, A.Decl_function (_, _, param_types, _, _, _)) ->
         let fn_name = Sym.pp_string fn_sym in
         let params =
           List.map
             (fun (param_sym, (_, ctype, _)) ->
                (Sym.pp_string param_sym, Sctypes.of_ctype ctype))
             (List.combine param_syms param_types)
         in
         (match List.find_opt (fun (_, opt) -> Option.is_none opt) params with
          | Some (bad_name, _) ->
            Printf.eprintf
              "bi-abd: skipping function %s (unsupported C type for parameter %s)\n"
              fn_name
              bad_name;
            None
          | None ->
            Some (fn_name, List.map (fun (n, opt) -> (n, Option.get opt)) params))
       | _ -> None)
    sigm.A.function_definitions


(** Resolve the CN runtime prefix (directory containing [include/] and
    [libcn_exec.a]). *)
let resolve_cn_runtime_prefix () : string =
  match Sys.getenv_opt "CN_RUNTIME_PREFIX" with
  | Some p when Sys.file_exists (Filename.concat p "include") -> p
  | _ ->
    let opam_rt =
      match Sys.getenv_opt "OPAM_SWITCH_PREFIX" with
      | Some p -> p ^ "/lib/cn/runtime"
      | None -> ""
    in
    if String.length opam_rt > 0 && Sys.file_exists opam_rt then
      opam_rt
    else (
      Printf.eprintf
        "Could not find CN runtime. Set CN_RUNTIME_PREFIX or install CN.\n\
         For development: CN_RUNTIME_PREFIX should contain include/ and libcn_exec.a\n";
      exit 1)


(** Compile and run the instrumented file as a subprocess.
    Returns the exit code. *)
let compile_and_run ~cc ~output_dir ~instrumented_file =
  let cn_runtime_prefix = resolve_cn_runtime_prefix () in
  let includes = "-I" ^ Filename.concat cn_runtime_prefix "include" in
  let lib_path = Filename.concat cn_runtime_prefix "libcn_exec.a" in
  let obj_file =
    Filename.concat
      output_dir
      (Filename.remove_extension (Filename.basename instrumented_file) ^ ".o")
  in
  let exe_file =
    Filename.concat
      output_dir
      (Filename.remove_extension (Filename.basename instrumented_file) ^ ".out")
  in
  let cflags = Option.value ~default:"" (Sys.getenv_opt "CFLAGS") in
  let cppflags = Option.value ~default:"" (Sys.getenv_opt "CPPFLAGS") in
  let flags = String.concat " " [ "-g"; cflags; cppflags ] in
  (* Compile *)
  let compile_cmd =
    Printf.sprintf "%s -c %s %s -o %s %s" cc flags includes obj_file instrumented_file
  in
  if Sys.command compile_cmd <> 0 then (
    Printf.eprintf "Failed to compile '%s'\n" instrumented_file;
    exit 1);
  (* Link *)
  let link_cmd =
    Printf.sprintf "%s %s %s -o %s %s %s -lm" cc flags includes exe_file obj_file lib_path
  in
  if Sys.command link_cmd <> 0 then (
    Printf.eprintf "Failed to link '%s'\n" instrumented_file;
    exit 1);
  (* Run as subprocess *)
  Sys.command exe_file


let generate_bi_abd
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
      fail_fast
      no_inherit_loc
      magic_comment_char_dollar
      allow_split_magic_comments
  =
  Cerb_debug.debug_level := debug_level;
  Pp.loc_pp := loc_pp;
  Pp.print_level := print_level;
  Sym.print_nums := print_sym_nums;
  Pp.print_timestamps := not no_timestamps;
  IndexTerms.use_vip := not dont_use_vip;
  Check.fail_fast := fail_fast;
  Diagnostics.diag_string := diag;
  Sym.executable_spec_enabled := true;
  let handle_error (e : TypeErrors.t) =
    let report = TypeErrors.pp_message e.msg in
    Pp.error e.loc report.short (Option.to_list report.descr);
    match e.msg with TypeErrors.Unsupported _ -> exit 2 | _ -> exit 1
  in
  let filename = Common.there_can_only_be_one filename in
  let basefile = Filename.basename filename in
  let pp_file = Filename.temp_file "cn_" basefile in
  let out_file = Fulminate.get_instrumented_filename basefile in
  Common.with_well_formedness_check
    ~filename
    ~cc
    ~macros:(("__CN_INSTRUMENT", None) :: macros)
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
    ~save_cpp:(Some pp_file)
    ~disable_linemarkers:false
    ~skip_label_inlining:true
    ~handle_error
    ~f:(fun ~cabs_tunit ~prog5 ~ail_prog ~statement_locs:_ ~paused:_ ->
      let _startup_sym, sigm = ail_prog in
      let struct_defs = extract_struct_defs sigm in
      let pred_defs = extract_pred_defs prog5 in
      let function_args = extract_function_args sigm in
      let output_dir =
        Common.mk_dir_if_not_exist_maybe_tmp ~mktemp:true Instrument None
      in
      (* Step 1: Generate instrumented C *)
      Cerb_colour.without_colour
        (fun () ->
           (try
              Fulminate.main
                ~without_ownership_checking:false
                ~without_loop_invariants:false
                ~with_loop_leak_checks:false
                ~without_lemma_checks:false
                ~exec_c_locs_mode:false
                ~correct_missing_ownership_mode:false
                ~experimental_ownership_stack_mode:false
                ~experimental_curly_braces:false
                ~with_testing:false
                ~bi_abductive:true
                ~skip_and_only:([], [])
                filename
                cc
                pp_file
                out_file
                output_dir
                cabs_tunit
                ail_prog
                prog5
            with
            | e -> Common.handle_error_with_user_guidance ~label:"CN-Exec" e);
           ())
        ();
      let instrumented_path = Filename.concat output_dir out_file in
      Printf.printf "Instrumented: %s\n" instrumented_path;
      (* Step 2: Compile and run *)
      Printf.printf "Compiling and running...\n%!";
      let exit_code =
        compile_and_run ~cc ~output_dir ~instrumented_file:instrumented_path
      in
      Printf.printf "Execution finished (exit code %d)\n%!" exit_code;
      (* Step 3: Run inference *)
      let summary_file = "cn_abd_summary.json" in
      let heap_file = "cn_abd_heap.jsonl" in
      if not (Sys.file_exists summary_file) then (
        Printf.eprintf "No summary file found at %s\n" summary_file;
        Ok ())
      else (
        Printf.printf "Running inference...\n%!";
        let config = Bi_abduction.Enumerator.default_config in
        let cn_runtime_prefix = resolve_cn_runtime_prefix () in
        let harness : Bi_abduction.Footprint.harness_ctx =
          { cc;
            output_dir;
            cn_runtime_prefix;
            filename;
            cabs_tunit;
            ail_prog = sigm;
            prog5
          }
        in
        let specs =
          Bi_abduction.Infer.infer
            ~config
            ~harness
            ~summary_file
            ~heap_file
            ~pred_defs
            ~struct_defs
            ~function_args
        in
        Printf.printf "\n";
        let doc = Bi_abduction.Infer.pp_suggestions specs in
        Pp.print stdout doc;
        Format.printf "@.";
        Ok ()))


let cmd =
  let open Term in
  let bi_abd_t =
    const generate_bi_abd
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
    $ Verify.Flags.fail_fast
    $ Common.Flags.no_inherit_loc
    $ Common.Flags.magic_comment_char_dollar
    $ Common.Flags.allow_split_magic_comments
  in
  let doc =
    "Push-button bi-abductive inference. Instruments [FILE] with bi-abductive execution \
     support, compiles and runs it, then analyses the output to suggest CN \
     specifications."
  in
  let info = Cmd.info "bi-abd" ~doc in
  Cmd.v info bi_abd_t

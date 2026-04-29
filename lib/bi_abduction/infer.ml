(** Top-level inference orchestrator.

    Pipeline: parse summary + heap → group by function → for each function:
    enumerate qualifiers → run the predicate-footprint harness → compute
    cover → format suggestions.

    Owned qualifiers get analytical footprints from [Footprint.owned_footprint].
    Predicate qualifiers get their footprints from a generated C harness
    ([Fp_codegen]) compiled and run by [Fp_runner].

    Debug output via [Pp.debug]:
    - Level 2: pipeline stages
    - Level 3: data point details, representative selection
    - Level 4: enumeration / harness results
    - Level 5: per-qualifier footprints, cover steps *)

module CF = Cerb_frontend
module StdList = Stdlib.List
module Int64Set = Data_point.Int64Set

type inferred_qualifiers =
  { pre : Qualifier.t list;
    post : Qualifier.t list
  }

type inferred_spec =
  { function_name : string;
    qualifiers : inferred_qualifiers option (** [None] when cover failed. *)
  }

type harness_ctx =
  { cc : string;
    output_dir : string;
    cn_runtime_prefix : string;
    filename : string;
    cabs_tunit : CF.Cabs.translation_unit;
    ail_prog : CF.GenTypes.genTypeCategory CF.AilSyntax.sigma;
    prog5 : unit Mucore.file
  }

let is_predicate_qualifier (q : Qualifier.t) : bool =
  match q with
  | Request.P { name = PName _; _ } -> true
  | _ -> false


(** Run the inference pipeline for one function. *)
let infer_function
      ~(config : Enumerator.config)
      ~(harness : harness_ctx)
      ~(pred_defs : Definition.Predicate.t Sym.Map.t)
      ~(signature_args : (string * Sctypes.t) list)
      ~(pre_heap_words : (int64 * int64) list)
      ~(post_heap_words : (int64 * int64) list)
      ~(func_name : string)
      ~(dps : Data_point.data_point list)
  : inferred_spec
  =
  let open Pp in
  Pp.debug 2 (lazy (headline ("bi-abd: inferring specs for " ^ func_name)));
  Pp.debug 3 (lazy (item "data points" (Pp.int (StdList.length dps))));
  let loc = Locations.other __FUNCTION__ in
  (* Baseline: pick the dp with the richest missing set as the representative. *)
  let representative_dp =
    StdList.fold_left
      (fun best (dp : Data_point.data_point) ->
         let n = StdList.length dp.body_missing + StdList.length dp.post_remaining in
         let best_n =
           StdList.length best.Data_point.body_missing
           + StdList.length best.Data_point.post_remaining
         in
         if n > best_n then dp else best)
      (StdList.hd dps)
      dps
  in
  Pp.debug
    3
    (lazy
      (let body_n = StdList.length representative_dp.Data_point.body_missing in
       let post_n = StdList.length representative_dp.Data_point.post_remaining in
       item
         "representative data point"
         (!^"body_missing:" ^^^ Pp.int body_n ^^^ !^"post_remaining:" ^^^ Pp.int post_n)));
  Pp.debug
    3
    (lazy
      (item
         "variables"
         (separate_map
            (comma ^^ space)
            (fun (v : Data_point.var_binding) ->
               !^(v.name)
               ^^^ !^"="
               ^^^ !^(Printf.sprintf "0x%Lx" v.value)
               ^^^ !^(Printf.sprintf "(%d bytes)" v.size))
            representative_dp.Data_point.pre_vars)));
  let signature_arg_type name : Sctypes.t = StdList.assoc name signature_args in
  let arg_of_var (v : Data_point.var_binding) : Enumerator.arg =
    let sym = Sym.fresh v.name in
    match signature_arg_type v.name with
    | Sctypes.Pointer ((Sctypes.Void | Sctypes.Function _) as _ct) ->
      { sym; bt = BaseTypes.Loc (); owned_ct = None }
    | Sctypes.Pointer ct -> { sym; bt = BaseTypes.Loc (); owned_ct = Some ct }
    | Sctypes.Integer _ | Sctypes.Byte ->
      { sym; bt = BaseTypes.Integer; owned_ct = None }
    | ct ->
      failwith
        (Printf.sprintf
           "bi-abduction: unsupported argument type for %s in %s: %s"
           v.name
           func_name
           (Pp.plain (Sctypes.pp ct)))
  in
  let args = StdList.map arg_of_var representative_dp.pre_vars in
  let candidates_raw = Enumerator.enumerate ~config ~args ~pred_defs ~loc in
  Pp.debug
    4
    (lazy
      (item
         "candidates (raw)"
         (Pp.int (StdList.length candidates_raw) ^^^ !^"qualifiers")));
  StdList.iter
    (fun q -> Pp.debug 5 (lazy (item "  candidate" (Qualifier.pp q))))
    candidates_raw;
  let candidates_indexed = StdList.mapi (fun i q -> (i, q)) candidates_raw in
  let pred_qualifiers =
    StdList.filter (fun (_, q) -> is_predicate_qualifier q) candidates_indexed
  in
  Pp.debug
    4
    (lazy
      (item
         "predicate qualifiers"
         (Pp.int (StdList.length pred_qualifiers) ^^^ !^"to harness")));
  let run_harness ~tag ~heap_words : Fp_table.t =
    if StdList.length pred_qualifiers = 0 then
      Fp_table.empty
    else (
      let codegen_input : Fp_codegen.input =
        { filename = harness.filename;
          cabs_tunit = harness.cabs_tunit;
          ail_prog = harness.ail_prog;
          prog5 = harness.prog5;
          pred_defs;
          representative_dp;
          heap_words;
          qualifiers = pred_qualifiers;
          output_json_path = "" (* set inside Fp_runner.run *)
        }
      in
      Fp_runner.run
        ~cc:harness.cc
        ~output_dir:harness.output_dir
        ~cn_runtime_prefix:harness.cn_runtime_prefix
        ~func_name
        ~tag
        codegen_input)
  in
  let pre_fp_table = run_harness ~tag:"pre" ~heap_words:pre_heap_words in
  let post_fp_table = run_harness ~tag:"post" ~heap_words:post_heap_words in
  let footprint_of ~(fp_table : Fp_table.t) (q_idx, q) : Int64Set.t option =
    match q with
    | Request.P { name = Owned _; _ } -> Footprint.compute q representative_dp
    | Request.P { name = PName _; _ } ->
      (match Fp_table.find fp_table q_idx with
       | Some fp -> fp
       | None -> None)
    | _ -> None
  in
  let infer_function_inner (phase : [ `Pre | `Post ]) : Cover.cover_result =
    let phase_label, select_missing, fp_table =
      match phase with
      | `Pre ->
        ("pre", (fun (dp : Data_point.data_point) -> dp.body_missing), pre_fp_table)
      | `Post ->
        ( "post",
          (fun (dp : Data_point.data_point) -> dp.post_remaining),
          post_fp_table )
    in
    let must_cover = Data_point.missing_addr_set (select_missing representative_dp) in
    Pp.debug
      3
      (lazy
        (item
           (phase_label ^ " must-cover")
           (Pp.int (Int64Set.cardinal must_cover) ^^^ !^"bytes")));
    let candidates =
      StdList.filter_map
        (fun (q_idx, q) ->
           match footprint_of ~fp_table (q_idx, q) with
           | Some fp when not (Int64Set.is_empty (Int64Set.inter fp must_cover)) ->
             let covers = Int64Set.cardinal (Int64Set.inter fp must_cover) in
             Pp.debug
               5
               (lazy
                 (item
                    ("  " ^ phase_label ^ " footprint")
                    (Qualifier.pp q
                     ^^^ !^"->"
                     ^^^ Pp.int (Int64Set.cardinal fp)
                     ^^^ !^"bytes,"
                     ^^^ Pp.int covers
                     ^^^ !^"covering must")));
             Some { Cover.qualifier = q; footprint = fp }
           | _ -> None)
        candidates_indexed
    in
    Pp.debug
      4
      (lazy
        (item
           (phase_label ^ " candidates with footprints")
           (Pp.int (StdList.length candidates))));
    let result = Cover.cover ~must_cover ~candidates in
    Pp.debug
      3
      (lazy
        (let n_sel = StdList.length result.selected in
         let n_uncov = Int64Set.cardinal result.uncovered in
         item
           (phase_label ^ " cover result")
           (Pp.int n_sel ^^^ !^"selected," ^^^ Pp.int n_uncov ^^^ !^"uncovered")));
    result
  in
  let pre_result = infer_function_inner `Pre in
  let post_result = infer_function_inner `Post in
  let qualifiers =
    if Int64Set.is_empty pre_result.uncovered && Int64Set.is_empty post_result.uncovered
    then
      Some { pre = pre_result.selected; post = post_result.selected }
    else
      None
  in
  { function_name = func_name; qualifiers }


(** Main entry: parse the summary + heap files and infer per function. *)
let infer
      ~(config : Enumerator.config)
      ~(harness : harness_ctx)
      ~(summary_file : string)
      ~(heap_file : string)
      ~(pred_defs : Definition.Predicate.t Sym.Map.t)
      ~(struct_defs : (Id.t * Sctypes.t) list Sym.Map.t)
      ~(function_args : (string * (string * Sctypes.t) list) list)
  : inferred_spec list
  =
  let open Pp in
  Pp.debug 2 (lazy (headline "bi-abd: starting inference"));
  Pp.debug 2 (lazy (Pp.item "bi-abd: parsing" (Pp.string summary_file)));
  let execution_data = Data_point.parse_summary_json summary_file in
  Pp.debug 2 (lazy (Pp.item "bi-abd: parsing" (Pp.string heap_file)));
  let pre_dumps, post_dumps = Data_point.parse_heap_jsonl heap_file in
  Pp.debug
    3
    (lazy
      (Pp.item
         "heap dumps"
         (Pp.string
            (Printf.sprintf
               "pre:%d post:%d"
               (StdList.length pre_dumps)
               (StdList.length post_dumps)))));
  (* Run the harness once per phase against its matching heap snapshot:
     [pre_heap_words] for the body_missing (precondition) phase,
     [post_heap_words] for the post_remaining (postcondition) phase.
     For functions that mutate the heap (e.g. constructors, setters)
     these snapshots differ, so reusing one for both phases would either
     reject or mis-shape the postcondition predicates. *)
  let pre_heap_words = Data_point.flatten_heap_dumps pre_dumps in
  let post_heap_words = Data_point.flatten_heap_dumps post_dumps in
  Pp.debug
    2
    (lazy
      (item
         "input"
         (Pp.int (StdList.length execution_data.data_points)
          ^^^ !^"data points,"
          ^^^ Pp.int (Sym.Map.cardinal pred_defs)
          ^^^ !^"predicates,"
          ^^^ Pp.int (Sym.Map.cardinal struct_defs)
          ^^^ !^"struct types")));
  let grouped = Data_point.group_by_function execution_data.data_points in
  Pp.debug
    2
    (lazy
      (item
         "functions"
         (separate_map
            (comma ^^ space)
            (fun (name, dps) ->
               !^name ^^^ !^(Printf.sprintf "(%d calls)" (StdList.length dps)))
            grouped)));
  StdList.filter_map
    (fun (func_name, dps) ->
       match StdList.assoc_opt func_name function_args with
       | None ->
         Printf.eprintf
           "bi-abd: skipping %s (no signature info available)\n"
           func_name;
         None
       | Some signature_args ->
         Some
           (infer_function
              ~config
              ~harness
              ~pred_defs
              ~signature_args
              ~pre_heap_words
              ~post_heap_words
              ~func_name
              ~dps))
    grouped


(** Pretty-print inferred specifications as CN annotation suggestions. *)
let pp_suggestions (specs : inferred_spec list) : Pp.document =
  let open Pp in
  let pp_qualifiers label qs =
    match qs with
    | [] -> Pp.empty
    | _ ->
      string (Printf.sprintf "  /* Suggested %s additions: */" label)
      ^^ hardline
      ^^ separate
           hardline
           (StdList.map (fun q -> string "  take _ = " ^^ Qualifier.pp q ^^ semi) qs)
  in
  StdList.map
    (fun spec ->
       let header =
         string (Printf.sprintf "/* Function: %s */" spec.function_name) ^^ hardline
       in
       match spec.qualifiers with
       | None -> header ^^ string "  /* inference failed */"
       | Some { pre; post } ->
         let pre_doc = pp_qualifiers "precondition" pre in
         let post_doc =
           match post with
           | [] -> Pp.empty
           | _ -> hardline ^^ pp_qualifiers "postcondition" post
         in
         header ^^ pre_doc ^^ post_doc)
    specs
  |> separate (hardline ^^ hardline)

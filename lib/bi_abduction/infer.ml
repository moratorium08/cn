(** Top-level inference orchestrator.

    Pipeline: parse summary + heap → group by function → for each function:
    enumerate qualifiers → run the predicate-footprint harness → compute
    cover → format suggestions.

    Owned qualifiers get analytical footprints from [Footprint.owned_footprint].
    Predicate qualifiers get their footprints from a generated C harness
    ([Fp_codegen]) compiled and run by [Fp_runner].

    Inference is data-relative across *all* activations (paper, condition
    (†)): a candidate must evaluate (F ≠ ⊥) on every data point, stay within
    every data point's upper bound B, and the selected candidates must cover
    every data point's lower bound A disjointly.

    Debug output via [Pp.debug]:
    - Level 2: pipeline stages
    - Level 3: per-data-point details, must-cover sets, cover results
    - Level 4: enumeration / harness results, rejected candidates
    - Level 5: per-qualifier footprints, cover steps *)

module StdList = Stdlib.List
module Int64Set = Data_point.Int64Set
module IntMap = Data_point.IntMap

type inferred_qualifiers =
  { pre : Qualifier.t list;
    post : Qualifier.t list
  }

type inferred_spec =
  { function_name : string;
    qualifiers : inferred_qualifiers option (** [None] when cover failed. *)
  }

let is_predicate_qualifier (q : Qualifier.t) : bool =
  match Qualifier.singleton_req q with
  | Some (Request.P { name = PName _; _ }) -> true
  | _ -> false


(** Run the inference pipeline for one function. *)
let infer_function
      ~(config : Enumerator.config)
      ~(harness : Footprint.harness_ctx)
      ~(pred_defs : Definition.Predicate.t Sym.Map.t)
      ~(signature_args : (string * Sctypes.t) list)
      ~(pre_heaps : Data_point.heap_dumps_by_dp)
      ~(post_heaps : Data_point.heap_dumps_by_dp)
      ~(func_name : string)
      ~(dps : Data_point.data_point list)
  : inferred_spec
  =
  let open Pp in
  Pp.debug 2 (lazy (headline ("bi-abd: inferring specs for " ^ func_name)));
  Pp.debug 3 (lazy (item "data points" (Pp.int (StdList.length dps))));
  let loc = Locations.other __FUNCTION__ in
  StdList.iter
    (fun (dp : Data_point.data_point) ->
       Pp.debug
         3
         (lazy
           (item
              (Printf.sprintf "dp %d" dp.dp_idx)
              (!^(Printf.sprintf
                    "A: %d entries, L: %d entries;"
                    (StdList.length dp.body_missing)
                    (StdList.length dp.post_remaining))
               ^^^ separate_map
                     (comma ^^ space)
                     (fun (v : Data_point.var_binding) ->
                        !^(v.name) ^^^ !^"=" ^^^ !^(Printf.sprintf "0x%Lx" v.value))
                     dp.pre_vars))))
    dps;
  let signature_arg_type name : Sctypes.t = StdList.assoc name signature_args in
  let arg_of_var (v : Data_point.var_binding) : Enumerator.arg =
    let sym = Sym.fresh v.name in
    match signature_arg_type v.name with
    | Sctypes.Pointer ((Sctypes.Void | Sctypes.Function _) as _ct) ->
      { sym; bt = BaseTypes.Loc (); owned_ct = None }
    | Sctypes.Pointer ct -> { sym; bt = BaseTypes.Loc (); owned_ct = Some ct }
    | Sctypes.Integer _ | Sctypes.Byte -> { sym; bt = BaseTypes.Integer; owned_ct = None }
    | ct ->
      failwith
        (Printf.sprintf
           "bi-abduction: unsupported argument type for %s in %s: %s"
           v.name
           func_name
           (Pp.plain (Sctypes.pp ct)))
  in
  (* All activations of a function share its signature; take scope from the
     first data point. *)
  let args = StdList.map arg_of_var (StdList.hd dps).pre_vars in
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
  (* The harness sweeps over every activation, each against its own heap
     snapshot; results are keyed by (q_idx, dp_idx). *)
  let run_harness ~tag ~(heaps : Data_point.heap_dumps_by_dp) : Fp_table.t =
    let data_points =
      StdList.map
        (fun (dp : Data_point.data_point) ->
           { Fp_codegen.dp_idx = dp.dp_idx;
             dp;
             heap_words = Data_point.heap_words_for_dp heaps dp.dp_idx
           })
        dps
    in
    Footprint.compute_predicate_table
      ~harness
      ~tag
      ~func_name
      ~pred_defs
      ~data_points
      ~qualifiers:pred_qualifiers
  in
  let pre_fp_table = run_harness ~tag:"pre" ~heaps:pre_heaps in
  let post_fp_table = run_harness ~tag:"post" ~heaps:post_heaps in
  (* Sandwich upper bound: an inferred assertion is *-conjoined with the
     user's specification, so on every data point its footprint must avoid
     what the activation already owned after the user precondition
     (owned_pre = user footprint + parameter/local cells); F ⊆ B_j reads as
     F ∩ owned_pre_j = ∅.  (At post this uses the pre snapshot as an
     approximation of the user's postcondition footprint.) *)
  let owned_pre_sets : Int64Set.t IntMap.t =
    StdList.fold_left
      (fun acc (dp : Data_point.data_point) ->
         IntMap.add dp.dp_idx (Data_point.missing_addr_set dp.owned_pre) acc)
      IntMap.empty
      dps
  in
  let infer_function_inner (phase : [ `Pre | `Post ]) : Cover.cover_result =
    let phase_label, select_missing, fp_table =
      match phase with
      | `Pre ->
        ("pre", (fun (dp : Data_point.data_point) -> dp.body_missing), pre_fp_table)
      | `Post ->
        ("post", (fun (dp : Data_point.data_point) -> dp.post_remaining), post_fp_table)
    in
    (* The lower bounds A_j, one per activation. *)
    let must_cover : Int64Set.t IntMap.t =
      StdList.fold_left
        (fun acc (dp : Data_point.data_point) ->
           IntMap.add dp.dp_idx (Data_point.missing_addr_set (select_missing dp)) acc)
        IntMap.empty
        dps
    in
    Pp.debug
      3
      (lazy
        (item
           (phase_label ^ " must-cover")
           (separate_map
              (comma ^^ space)
              (fun (dp_idx, a) ->
                 !^(Printf.sprintf "dp %d: %d bytes" dp_idx (Int64Set.cardinal a)))
              (IntMap.bindings must_cover))));
    (* Condition (†), applied per candidate:
       - F(q, d_j) ≠ ⊥ for every data point j (a qualifier that fails to
         evaluate on any observed execution cannot be part of the spec);
       - F(q, d_j) ∩ owned_pre_j = ∅ for every j (upper bound);
       - it helps somewhere: Σ_j |F(q, d_j) ∩ A_j| > 0 (removable
         heuristic; base-case activations with A_j = ∅ impose no demand). *)
    let candidates =
      StdList.filter_map
        (fun (q_idx, q) ->
           let fps =
             StdList.map
               (fun (dp : Data_point.data_point) ->
                  (dp.dp_idx, Footprint.lookup ~dp ~fp_table (q_idx, q)))
               dps
           in
           if StdList.exists (fun (_, fp) -> Option.is_none fp) fps then (
             Pp.debug
               5
               (lazy
                 (item
                    ("  " ^ phase_label ^ " rejected (undefined on some dp)")
                    (Qualifier.pp q)));
             None)
           else (
             let footprints =
               StdList.fold_left
                 (fun acc (dp_idx, fp) -> IntMap.add dp_idx (Option.get fp) acc)
                 IntMap.empty
                 fps
             in
             let overlaps_owned =
               IntMap.exists
                 (fun dp_idx fp ->
                    match IntMap.find_opt dp_idx owned_pre_sets with
                    | None -> false
                    | Some owned -> not (Int64Set.is_empty (Int64Set.inter owned fp)))
                 footprints
             in
             let help =
               IntMap.fold
                 (fun dp_idx fp acc ->
                    match IntMap.find_opt dp_idx must_cover with
                    | None -> acc
                    | Some a -> acc + Int64Set.cardinal (Int64Set.inter a fp))
                 footprints
                 0
             in
             if overlaps_owned then (
               Pp.debug
                 4
                 (lazy
                   (item
                      ("  " ^ phase_label ^ " rejected (overlaps user-owned)")
                      (Qualifier.pp q)));
               None)
             else if help = 0 then
               None
             else (
               Pp.debug
                 5
                 (lazy
                   (item
                      ("  " ^ phase_label ^ " footprint")
                      (Qualifier.pp q
                       ^^^ !^"->"
                       ^^^ separate_map
                             (comma ^^ space)
                             (fun (dp_idx, fp) ->
                                !^(Printf.sprintf
                                     "dp %d: %d bytes"
                                     dp_idx
                                     (Int64Set.cardinal fp)))
                             (IntMap.bindings footprints))));
               Some { Cover.qualifier = q; footprints })))
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
         let n_uncov =
           IntMap.fold (fun _ s acc -> acc + Int64Set.cardinal s) result.uncovered 0
         in
         item
           (phase_label ^ " cover result")
           (Pp.int n_sel ^^^ !^"selected," ^^^ Pp.int n_uncov ^^^ !^"uncovered")));
    result
  in
  let pre_result = infer_function_inner `Pre in
  let post_result = infer_function_inner `Post in
  let covered (r : Cover.cover_result) =
    IntMap.for_all (fun _ s -> Int64Set.is_empty s) r.uncovered
  in
  let qualifiers =
    if covered pre_result && covered post_result then
      Some { pre = pre_result.selected; post = post_result.selected }
    else
      None
  in
  { function_name = func_name; qualifiers }


(** Main entry: parse the summary + heap files and infer per function. *)
let infer
      ~(config : Enumerator.config)
      ~(harness : Footprint.harness_ctx)
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
  (* Heap snapshots are keyed per activation (dp) and per phase: the pre
     snapshot approximates H_entry for the body_missing (precondition)
     phase, the post snapshot is H_exit for the post_remaining
     (postcondition) phase.  Distinct activations may reuse the same stack
     addresses with different contents, so snapshots are never merged
     across dps. *)
  let pre_heaps, post_heaps = Data_point.parse_heap_jsonl heap_file in
  Pp.debug
    3
    (lazy
      (Pp.item
         "heap dumps"
         (Pp.string
            (Printf.sprintf
               "pre: %d dps, post: %d dps"
               (Data_point.IntMap.cardinal pre_heaps)
               (Data_point.IntMap.cardinal post_heaps)))));
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
         Printf.eprintf "bi-abd: skipping %s (no signature info available)\n" func_name;
         None
       | Some signature_args ->
         Some
           (infer_function
              ~config
              ~harness
              ~pred_defs
              ~signature_args
              ~pre_heaps
              ~post_heaps
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
           (StdList.map (fun q -> string "  " ^^ nest 2 (Qualifier.pp_takes q)) qs)
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

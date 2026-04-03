(** Top-level inference orchestrator.

    Pipeline: parse summary + heap → group by function → for each function:
    build memory graph → enumerate qualifiers → compute footprints →
    run cover algorithm → format as CN annotation suggestions. *)

module StdList = Stdlib.List
module Int64Set = Data_point.Int64Set

type address_analysis =
  { var_name : string;
    var_addr : int64;
    missing_range : int  (* number of contiguous missing bytes from var_addr *)
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

(** Analyse which variable addresses overlap with missing address sets.
    For each variable, count missing bytes within a reasonable range
    of the variable's address (up to 4096 bytes forward, covering typical
    struct sizes and linked structures). *)
let analyse_missing_vars
      (vars : Data_point.var_binding list)
      (missing : Data_point.missing_entry list)
  : address_analysis list
  =
  let missing_addrs = Data_point.missing_addr_set missing in
  let max_range = 4096 in
  StdList.filter_map
    (fun (v : Data_point.var_binding) ->
       (* Count missing bytes in [v.value, v.value + max_range) *)
       let count = ref 0 in
       for offset = 0 to max_range - 1 do
         let addr = Int64.add v.value (Int64.of_int offset) in
         if Int64Set.mem addr missing_addrs then
           count := !count + 1
       done;
       if !count > 0 then
         Some { var_name = v.name; var_addr = v.value; missing_range = !count }
       else
         None)
    vars

(** Build struct layouts from struct definitions.
    Maps struct tag → list of (field_id, byte_offset, byte_size). *)
let build_struct_layouts
      (struct_defs : (Id.t * Sctypes.t) list Sym.Map.t)
  : (Id.t * int * int) list Sym.Map.t
  =
  Sym.Map.map
    (fun fields ->
       let _, rev_layout =
         StdList.fold_left
           (fun (running_offset, acc) (field_id, field_ct) ->
              let size = Memory.size_of_ctype field_ct in
              let align = Memory.align_of_ctype field_ct in
              let aligned_offset =
                if align > 0 then
                  let rem = running_offset mod align in
                  if rem = 0 then running_offset
                  else running_offset + (align - rem)
                else
                  running_offset
              in
              (aligned_offset + size, (field_id, aligned_offset, size) :: acc))
           (0, []) fields
       in
       StdList.rev rev_layout)
    struct_defs

(** Compute the must-cover set from data points for a function.
    Uses union of missing addresses across all data points so that
    addresses seen in any execution are considered. *)
let must_cover_set (dps : Data_point.data_point list) : Int64Set.t =
  StdList.fold_left
    (fun acc (dp : Data_point.data_point) ->
       Int64Set.union acc (Data_point.missing_addr_set dp.Data_point.pre_missing))
    Int64Set.empty dps

(** Compute the must-cover set for post-conditions. *)
let post_must_cover_set (dps : Data_point.data_point list) : Int64Set.t =
  StdList.fold_left
    (fun acc (dp : Data_point.data_point) ->
       Int64Set.union acc (Data_point.missing_addr_set dp.Data_point.post_missing))
    Int64Set.empty dps

(** Run the inference pipeline for one function. *)
let infer_function
      ~(config : Enumerator.config)
      ~(pred_defs : Definition.Predicate.t Sym.Map.t)
      ~(struct_defs : (Id.t * Sctypes.t) list Sym.Map.t)
      ~(heap_lookup : int64 -> int64 option)
      ~(func_name : string)
      ~(dps : Data_point.data_point list)
  : inferred_spec
  =
  let loc = Locations.other __FUNCTION__ in
  let struct_layouts = build_struct_layouts struct_defs in
  (* Pick the data point with the most missing addresses as representative.
     For recursive functions, base cases (e.g., NULL) have empty missing sets,
     so we need the call with the richest data. *)
  let representative_dp =
    StdList.fold_left
      (fun best (dp : Data_point.data_point) ->
         let n =
           StdList.length dp.pre_missing + StdList.length dp.post_missing
         in
         let best_n =
           StdList.length best.Data_point.pre_missing
           + StdList.length best.Data_point.post_missing
         in
         if n > best_n then dp else best)
      (StdList.hd dps)
      dps
  in
  let graph = Memory_graph.build
    ~dp:representative_dp
    ~heap_lookup
    ~struct_layouts
  in
  (* Extract function arguments as (Sym.t * BaseTypes.t) pairs *)
  let args =
    StdList.map
      (fun (v : Data_point.var_binding) ->
         let sym = Sym.fresh v.name in
         let bt : BaseTypes.t =
           if v.size = 8 then Loc ()
           else Integer
         in
         (sym, bt))
      representative_dp.pre_vars
  in
  (* Build variable name → address mapping for predicate connectivity check *)
  let var_addrs =
    StdList.map
      (fun (v : Data_point.var_binding) -> (v.name, v.value))
      representative_dp.Data_point.pre_vars
  in
  (* Enumerate candidate qualifiers *)
  let candidates = Enumerator.enumerate
    ~config ~args ~pred_defs ~struct_defs ~graph ~var_addrs ~loc
  in
  (* Compute footprints for pre-condition candidates *)
  let pre_must = must_cover_set dps in
  let pre_candidates =
    StdList.filter_map
      (fun q ->
         match Footprint.compute_with_graph q representative_dp graph ~struct_layouts with
         | Some fp when not (Int64Set.is_empty (Int64Set.inter fp pre_must)) ->
           Some { Cover.qualifier = q; footprint = fp }
         | _ -> None)
      candidates
  in
  let pre_result = Cover.cover ~must_cover:pre_must ~candidates:pre_candidates in
  (* Compute footprints for post-condition candidates *)
  let post_must = post_must_cover_set dps in
  let post_candidates =
    StdList.filter_map
      (fun q ->
         match Footprint.compute_with_graph q representative_dp graph ~struct_layouts with
         | Some fp when not (Int64Set.is_empty (Int64Set.inter fp post_must)) ->
           Some { Cover.qualifier = q; footprint = fp }
         | _ -> None)
      candidates
  in
  let post_result = Cover.cover ~must_cover:post_must ~candidates:post_candidates in
  (* Analyse which variables' memory ranges are missing *)
  let pre_analysis =
    analyse_missing_vars representative_dp.Data_point.pre_vars
      representative_dp.pre_missing
  in
  let post_analysis =
    analyse_missing_vars representative_dp.pre_vars
      representative_dp.post_missing
  in
  { function_name = func_name;
    pre_qualifiers = pre_result.selected;
    post_qualifiers = post_result.selected;
    pre_uncovered = pre_result.uncovered;
    post_uncovered = post_result.uncovered;
    pre_analysis;
    post_analysis
  }

(** Main entry point: run inference on execution data. *)
let infer
      ~(config : Enumerator.config)
      ~(execution_data : Data_point.execution_data)
      ~(heap_lookup : int64 -> int64 option)
      ~(pred_defs : Definition.Predicate.t Sym.Map.t)
      ~(struct_defs : (Id.t * Sctypes.t) list Sym.Map.t)
  : inferred_spec list
  =
  let grouped = Data_point.group_by_function execution_data.data_points in
  StdList.map
    (fun (func_name, dps) ->
       infer_function ~config ~pred_defs ~struct_defs
         ~heap_lookup ~func_name ~dps)
    grouped

(** Pretty-print inferred specifications as CN annotation suggestions. *)
let pp_suggestions (specs : inferred_spec list) : Pp.document =
  let open Pp in
  StdList.map
    (fun spec ->
       let pre_doc =
         match spec.pre_qualifiers with
         | [] -> Pp.empty
         | _ ->
           string "  /* Suggested precondition additions: */" ^^ hardline ^^
           separate hardline
             (StdList.map
                (fun q -> string "  take _ = " ^^ Qualifier.pp q ^^ semi)
                spec.pre_qualifiers)
       in
       let post_doc =
         match spec.post_qualifiers with
         | [] -> Pp.empty
         | _ ->
           hardline ^^
           string "  /* Suggested postcondition additions: */" ^^ hardline ^^
           separate hardline
             (StdList.map
                (fun q -> string "  take _ = " ^^ Qualifier.pp q ^^ semi)
                spec.post_qualifiers)
       in
       let analysis_doc label analyses =
         match analyses with
         | [] -> Pp.empty
         | _ ->
           hardline ^^
           string (Printf.sprintf "  /* %s address analysis: */" label) ^^ hardline ^^
           separate hardline
             (StdList.map
                (fun (a : address_analysis) ->
                   string (Printf.sprintf
                     "  /*   %s (0x%Lx): %d bytes missing -> likely needs Owned<_>(%s) */"
                     a.var_name a.var_addr a.missing_range a.var_name))
                analyses)
       in
       let uncov_doc =
         let pre_n = Int64Set.cardinal spec.pre_uncovered in
         let post_n = Int64Set.cardinal spec.post_uncovered in
         if pre_n = 0 && post_n = 0 then Pp.empty
         else
           hardline ^^
           string (Printf.sprintf
             "  /* Warning: %d pre-addresses and %d post-addresses remain uncovered */"
             pre_n post_n)
       in
       string (Printf.sprintf "/* Function: %s */" spec.function_name) ^^
       hardline ^^ pre_doc ^^ post_doc ^^
       analysis_doc "Pre" spec.pre_analysis ^^
       analysis_doc "Post" spec.post_analysis ^^
       uncov_doc)
    specs
  |> separate (hardline ^^ hardline)

(** Run inference from file paths (convenience entry point). *)
let infer_from_files
      ~(config : Enumerator.config)
      ~(summary_file : string)
      ~(heap_file : string)
      ~(pred_defs : Definition.Predicate.t Sym.Map.t)
      ~(struct_defs : (Id.t * Sctypes.t) list Sym.Map.t)
  : inferred_spec list
  =
  let execution_data = Data_point.parse_summary_json summary_file in
  let heap_dumps = Data_point.parse_heap_jsonl heap_file in
  let heap_lookup = Data_point.heap_lookup heap_dumps in
  infer ~config ~execution_data ~heap_lookup ~pred_defs ~struct_defs

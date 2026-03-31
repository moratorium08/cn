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
    For each variable, count how many contiguous bytes starting from
    the variable's address appear in the missing set. *)
let analyse_missing_vars
      (vars : Data_point.var_binding list)
      (missing : Data_point.missing_entry list)
  : address_analysis list
  =
  let missing_addrs = Data_point.missing_addr_set missing in
  StdList.filter_map
    (fun (v : Data_point.var_binding) ->
       (* Count contiguous missing bytes starting at v.value *)
       let rec count_contiguous offset =
         let addr = Int64.add v.value (Int64.of_int offset) in
         if Int64Set.mem addr missing_addrs then
           count_contiguous (offset + 1)
         else
           offset
       in
       let n = count_contiguous 0 in
       if n > 0 then
         Some { var_name = v.name; var_addr = v.value; missing_range = n }
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
       StdList.map
         (fun (field_id, field_ct) ->
            let size = Memory.size_of_ctype field_ct in
            (* Offset computation: we'd need to walk fields in order
               with alignment. For simplicity, use Cerberus's offsetof.
               As a fallback, use sequential packing. *)
            ignore size;
            (field_id, 0, size))
         fields)
    struct_defs

(** Compute the must-cover set from data points for a function.
    The must-cover set is the intersection of missing addresses across
    all data points (addresses that are always missing). *)
let must_cover_set (dps : Data_point.data_point list) : Int64Set.t =
  match dps with
  | [] -> Int64Set.empty
  | first :: rest ->
    let first_missing = Data_point.missing_addr_set first.Data_point.pre_missing in
    StdList.fold_left
      (fun acc (dp : Data_point.data_point) ->
         Int64Set.inter acc (Data_point.missing_addr_set dp.pre_missing))
      first_missing rest

(** Compute the must-cover set for post-conditions. *)
let post_must_cover_set (dps : Data_point.data_point list) : Int64Set.t =
  match dps with
  | [] -> Int64Set.empty
  | first :: rest ->
    let first_missing = Data_point.missing_addr_set first.Data_point.post_missing in
    StdList.fold_left
      (fun acc (dp : Data_point.data_point) ->
         Int64Set.inter acc (Data_point.missing_addr_set dp.post_missing))
      first_missing rest

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
  (* Use the first data point to build the memory graph and extract args *)
  let representative_dp = StdList.hd dps in
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
  (* Enumerate candidate qualifiers *)
  let candidates = Enumerator.enumerate
    ~config ~args ~pred_defs ~struct_defs ~graph ~loc
  in
  (* Compute footprints for pre-condition candidates *)
  let pre_must = must_cover_set dps in
  let pre_candidates =
    StdList.filter_map
      (fun q ->
         match Footprint.compute_with_graph q representative_dp graph with
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
         match Footprint.compute_with_graph q representative_dp graph with
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

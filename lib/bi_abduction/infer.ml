(** Top-level inference orchestrator.

    Pipeline: parse summary + heap → group by function → for each function:
    build memory graph → enumerate qualifiers → compute footprints →
    run cover algorithm → format as CN annotation suggestions.

    Debug output via [Pp.debug]:
    - Level 2: pipeline stages
    - Level 3: data point details, representative selection
    - Level 4: graph stats, enumeration results
    - Level 5: per-qualifier footprints, cover steps *)

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

(** Compute the pre-condition must-cover set: union of body_missing
    (body auto-grants = precondition needs) across all data points. *)
let pre_must_cover_set (dps : Data_point.data_point list) : Int64Set.t =
  StdList.fold_left
    (fun acc (dp : Data_point.data_point) ->
       Int64Set.union acc (Data_point.missing_addr_set dp.body_missing))
    Int64Set.empty dps

(** Compute the post-condition must-cover set: union of post_remaining
    (leak check remainder = postcondition) across all data points. *)
let post_must_cover_set (dps : Data_point.data_point list) : Int64Set.t =
  StdList.fold_left
    (fun acc (dp : Data_point.data_point) ->
       Int64Set.union acc (Data_point.missing_addr_set dp.post_remaining))
    Int64Set.empty dps

(** Run the inference pipeline for one function. *)
let infer_function
      ~(config : Enumerator.config)
      ~(pred_defs : Definition.Predicate.t Sym.Map.t)
      ~(struct_defs : (Id.t * Sctypes.t) list Sym.Map.t)
      ~(function_args : (string * (string * Sctypes.t) list) list)
      ~(pre_heap_lookup : int64 -> int64 option)
      ~(post_heap_lookup : int64 -> int64 option)
      ~(func_name : string)
      ~(dps : Data_point.data_point list)
  : inferred_spec
  =
  let open Pp in
  Pp.debug 2 (lazy (headline ("bi-abd: inferring specs for " ^ func_name)));
  Pp.debug 3 (lazy (item "data points" (Pp.int (StdList.length dps))));
  let loc = Locations.other __FUNCTION__ in
  let struct_layouts = build_struct_layouts struct_defs in
  Pp.debug 4 (lazy (item "struct layouts"
    (Pp.int (Sym.Map.cardinal struct_layouts) ^^^ !^"struct types")));
  (* Pick the data point with the most missing addresses as representative.
     For recursive functions, base cases (e.g., NULL) have empty missing sets,
     so we need the call with the richest data. *)
  (* TODO (HK): Handle multiple data points at once *)
  let representative_dp =
    StdList.fold_left
      (fun best (dp : Data_point.data_point) ->
         let n =
           StdList.length dp.body_missing + StdList.length dp.post_remaining
         in
         let best_n =
           StdList.length best.Data_point.body_missing
           + StdList.length best.Data_point.post_remaining
         in
         if n > best_n then dp else best)
      (StdList.hd dps)
      dps
  in
  Pp.debug 3 (lazy begin
    let body_n = StdList.length representative_dp.Data_point.body_missing in
    let post_n = StdList.length representative_dp.Data_point.post_remaining in
    item "representative data point"
      (!^"body_missing:" ^^^ Pp.int body_n ^^^
       !^"post_remaining:" ^^^ Pp.int post_n)
  end);
  Pp.debug 3 (lazy begin
    item "variables"
      (separate_map (comma ^^ space)
         (fun (v : Data_point.var_binding) ->
            !^(v.name) ^^^ !^"=" ^^^ !^(Printf.sprintf "0x%Lx" v.value)
            ^^^ !^(Printf.sprintf "(%d bytes)" v.size))
         representative_dp.Data_point.pre_vars)
  end);
  let pre_missing_set = Data_point.missing_addr_set representative_dp.body_missing in
  let post_remaining_set = Data_point.missing_addr_set representative_dp.post_remaining in
  (* Build separate graphs for pre and post:
     - pre_graph  uses H_entry heap snapshot and body_missing as the missing set
     - post_graph uses H_exit  heap snapshot and post_remaining as the missing set *)
  let pre_graph = Memory_graph.build
    ~pre_vars:representative_dp.pre_vars
    ~missing_set:pre_missing_set
    ~heap_lookup:pre_heap_lookup
    ~struct_layouts
  in
  let post_graph = Memory_graph.build
    ~pre_vars:representative_dp.pre_vars
    ~missing_set:post_remaining_set
    ~heap_lookup:post_heap_lookup
    ~struct_layouts
  in
  Pp.debug 4 (lazy
    (item "pre memory graph"
       (!^(Printf.sprintf "%d nodes, %d anchors, %d missing"
              (Memory_graph.Int64Map.cardinal (Memory_graph.info pre_graph))
              (Int64Set.cardinal (Memory_graph.anchors pre_graph))
              (Int64Set.cardinal (Memory_graph.missing pre_graph))))));
  Pp.debug 4 (lazy
    (item "post memory graph"
       (!^(Printf.sprintf "%d nodes, %d anchors, %d missing"
              (Memory_graph.Int64Map.cardinal (Memory_graph.info post_graph))
              (Int64Set.cardinal (Memory_graph.anchors post_graph))
              (Int64Set.cardinal (Memory_graph.missing post_graph))))));
  let signature_args =
    Option.value ~default:[] (StdList.assoc_opt func_name function_args)
  in
  let signature_arg_type name = StdList.assoc_opt name signature_args in
  let arg_bt_of_ct (ct : Sctypes.t) : BaseTypes.t =
    match ct with
    | Sctypes.Pointer _ -> BaseTypes.Loc ()
    | _ -> Integer
  in
  let arg_of_var (v : Data_point.var_binding) : Enumerator.arg =
    let sym = Sym.fresh v.name in
    match signature_arg_type v.name with
    | Some (Sctypes.Pointer ((Sctypes.Void | Sctypes.Function _) as _ct)) ->
      { sym; bt = BaseTypes.Loc (); owned_ct = None }
    | Some (Sctypes.Pointer ct) ->
      { sym; bt = BaseTypes.Loc (); owned_ct = Some ct }
    | Some ct ->
      { sym; bt = arg_bt_of_ct ct; owned_ct = None }
    | None ->
      let bt : BaseTypes.t =
        if v.size = 8 then Loc ()
        else Integer
      in
      { sym; bt; owned_ct = None }
  in
  (* Extract function arguments with actual pointee types when available. *)
  let args =
    StdList.map arg_of_var representative_dp.pre_vars
  in
  (* Build variable name → address mapping for predicate connectivity check *)
  let var_addrs =
    StdList.map
      (fun (v : Data_point.var_binding) -> (v.name, v.value))
      representative_dp.Data_point.pre_vars
  in
  (* Enumerate candidate qualifiers separately for pre and post,
     using their respective heap graphs. *)
  let pre_candidates_raw = Enumerator.enumerate
    ~config ~args ~pred_defs ~struct_defs ~graph:pre_graph ~var_addrs ~loc
  in
  let post_candidates_raw = Enumerator.enumerate
    ~config ~args ~pred_defs ~struct_defs ~graph:post_graph ~var_addrs ~loc
  in
  Pp.debug 4 (lazy
    (item "pre candidates (raw)"
       (Pp.int (StdList.length pre_candidates_raw) ^^^ !^"qualifiers")));
  Pp.debug 4 (lazy
    (item "post candidates (raw)"
       (Pp.int (StdList.length post_candidates_raw) ^^^ !^"qualifiers")));
  StdList.iter (fun q ->
    Pp.debug 5 (lazy (item "  pre candidate" (Qualifier.pp q))))
    pre_candidates_raw;
  StdList.iter (fun q ->
    Pp.debug 5 (lazy (item "  post candidate" (Qualifier.pp q))))
    post_candidates_raw;
  (* Compute footprints for pre-condition candidates using pre_graph *)
  let pre_must = pre_must_cover_set dps in
  Pp.debug 3 (lazy
    (item "pre must-cover" (Pp.int (Int64Set.cardinal pre_must) ^^^ !^"bytes")));
  let pre_candidates =
    StdList.filter_map
      (fun q ->
         match Footprint.compute_with_graph q representative_dp pre_graph ~struct_layouts with
         | Some fp when not (Int64Set.is_empty (Int64Set.inter fp pre_must)) ->
           let covers = Int64Set.cardinal (Int64Set.inter fp pre_must) in
           Pp.debug 5 (lazy
             (item "  pre footprint"
                (Qualifier.pp q ^^^ !^"->" ^^^
                 Pp.int (Int64Set.cardinal fp) ^^^ !^"bytes," ^^^
                 Pp.int covers ^^^ !^"covering must")));
           Some { Cover.qualifier = q; footprint = fp }
         | _ -> None)
      pre_candidates_raw
  in
  Pp.debug 4 (lazy
    (item "pre candidates with footprints" (Pp.int (StdList.length pre_candidates))));
  let pre_result = Cover.cover ~must_cover:pre_must ~candidates:pre_candidates in
  Pp.debug 3 (lazy begin
    let n_sel = StdList.length pre_result.selected in
    let n_uncov = Int64Set.cardinal pre_result.uncovered in
    item "pre cover result"
      (Pp.int n_sel ^^^ !^"selected," ^^^
       Pp.int n_uncov ^^^ !^"uncovered")
  end);
  (* Compute footprints for post-condition candidates using post_graph *)
  let post_must = post_must_cover_set dps in
  Pp.debug 3 (lazy
    (item "post must-cover" (Pp.int (Int64Set.cardinal post_must) ^^^ !^"bytes")));
  let post_candidates =
    StdList.filter_map
      (fun q ->
         match Footprint.compute_with_graph q representative_dp post_graph ~struct_layouts with
         | Some fp when not (Int64Set.is_empty (Int64Set.inter fp post_must)) ->
           let covers = Int64Set.cardinal (Int64Set.inter fp post_must) in
           Pp.debug 5 (lazy
             (item "  post footprint"
                (Qualifier.pp q ^^^ !^"->" ^^^
                 Pp.int (Int64Set.cardinal fp) ^^^ !^"bytes," ^^^
                 Pp.int covers ^^^ !^"covering must")));
           Some { Cover.qualifier = q; footprint = fp }
         | _ -> None)
      post_candidates_raw
  in
  Pp.debug 4 (lazy
    (item "post candidates with footprints" (Pp.int (StdList.length post_candidates))));
  let post_result = Cover.cover ~must_cover:post_must ~candidates:post_candidates in
  Pp.debug 3 (lazy begin
    let n_sel = StdList.length post_result.selected in
    let n_uncov = Int64Set.cardinal post_result.uncovered in
    item "post cover result"
      (Pp.int n_sel ^^^ !^"selected," ^^^
       Pp.int n_uncov ^^^ !^"uncovered")
  end);
  (* Analyse which variables' memory ranges are missing *)
  let pre_analysis =
    analyse_missing_vars representative_dp.Data_point.pre_vars
      representative_dp.body_missing
  in
  let post_analysis =
    analyse_missing_vars representative_dp.pre_vars
      representative_dp.post_remaining
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
      ~(heap_dumps : Data_point.heap_dump list)
      ~(pred_defs : Definition.Predicate.t Sym.Map.t)
      ~(struct_defs : (Id.t * Sctypes.t) list Sym.Map.t)
      ~(function_args : (string * (string * Sctypes.t) list) list)
  : inferred_spec list
  =
  let open Pp in
  Pp.debug 2 (lazy (headline "bi-abd: starting inference"));
  Pp.debug 2 (lazy
    (item "input"
       (Pp.int (StdList.length execution_data.data_points) ^^^ !^"data points," ^^^
        Pp.int (Sym.Map.cardinal pred_defs) ^^^ !^"predicates," ^^^
        Pp.int (Sym.Map.cardinal struct_defs) ^^^ !^"struct types")));
  (* Build phase-specific heap lookups:
     pre  = Pre (H_entry: arg-neighborhood snapshots taken before body executes)
     post = Post (H_exit: leaked-address snapshots taken after body executes) *)
  let pre_heap_lookup =
    Data_point.heap_lookup_for_phases [Pre] heap_dumps
  in
  let post_heap_lookup =
    Data_point.heap_lookup_for_phases [Post] heap_dumps
  in
  let grouped = Data_point.group_by_function execution_data.data_points in
  Pp.debug 2 (lazy
    (item "functions"
       (separate_map (comma ^^ space)
          (fun (name, dps) ->
             !^name ^^^ !^(Printf.sprintf "(%d calls)" (StdList.length dps)))
          grouped)));
  StdList.map
    (fun (func_name, dps) ->
       infer_function ~config ~pred_defs ~struct_defs
         ~function_args ~pre_heap_lookup ~post_heap_lookup ~func_name ~dps)
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
      ~(function_args : (string * (string * Sctypes.t) list) list)
  : inferred_spec list
  =
  Pp.debug 2 (lazy (Pp.item "bi-abd: parsing" (Pp.string summary_file)));
  let execution_data = Data_point.parse_summary_json summary_file in
  Pp.debug 2 (lazy (Pp.item "bi-abd: parsing" (Pp.string heap_file)));
  let heap_dumps = Data_point.parse_heap_jsonl heap_file in
  Pp.debug 3 (lazy
    (Pp.item "heap dumps"
       (Pp.string (Printf.sprintf "%d entries" (StdList.length heap_dumps)))));
  infer ~config ~execution_data ~heap_dumps ~pred_defs ~struct_defs ~function_args

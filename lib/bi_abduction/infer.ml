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

type inferred_qualifiers =
  { pre : Qualifier.t list;
    post : Qualifier.t list
  }

type inferred_spec =
  { function_name : string;
    qualifiers : inferred_qualifiers option (** [None] when cover failed. *)
  }

(** Build struct layouts from struct definitions.
    Maps struct tag → list of (field_id, byte_offset, byte_size). *)
let build_struct_layouts (struct_defs : (Id.t * Sctypes.t) list Sym.Map.t)
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
                if align > 0 then (
                  let rem = running_offset mod align in
                  if rem = 0 then
                    running_offset
                  else
                    running_offset + (align - rem))
                else
                  running_offset
              in
              (aligned_offset + size, (field_id, aligned_offset, size) :: acc))
           (0, [])
           fields
       in
       StdList.rev rev_layout)
    struct_defs


(** Compute the pre-condition must-cover set: union of body_missing
    (body auto-grants = precondition needs) across all data points. *)
let pre_must_cover_set (dps : Data_point.data_point list) : Int64Set.t =
  StdList.fold_left
    (fun acc (dp : Data_point.data_point) ->
       Int64Set.union acc (Data_point.missing_addr_set dp.body_missing))
    Int64Set.empty
    dps


(** Compute the post-condition must-cover set: union of post_remaining
    (leak check remainder = postcondition) across all data points. *)
let post_must_cover_set (dps : Data_point.data_point list) : Int64Set.t =
  StdList.fold_left
    (fun acc (dp : Data_point.data_point) ->
       Int64Set.union acc (Data_point.missing_addr_set dp.post_remaining))
    Int64Set.empty
    dps


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
  Pp.debug
    4
    (lazy
      (item
         "struct layouts"
         (Pp.int (Sym.Map.cardinal struct_layouts) ^^^ !^"struct types")));
  (* Pick the data point with the most missing addresses as representative.
     For recursive functions, base cases (e.g., NULL) have empty missing sets,
     so we need the call with the richest data. *)
  (* TODO (HK): Handle multiple data points at once *)
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
  let pre_missing_set = Data_point.missing_addr_set representative_dp.body_missing in
  let post_remaining_set = Data_point.missing_addr_set representative_dp.post_remaining in
  (* Build separate graphs for pre and post:
     - pre_graph  uses H_entry heap snapshot and body_missing as the missing set
     - post_graph uses H_exit  heap snapshot and post_remaining as the missing set *)
  let pre_graph =
    Memory_graph.build
      ~pre_vars:representative_dp.pre_vars
      ~missing_set:pre_missing_set
      ~heap_lookup:pre_heap_lookup
      ~struct_layouts
  in
  let post_graph =
    Memory_graph.build
      ~pre_vars:representative_dp.pre_vars
      ~missing_set:post_remaining_set
      ~heap_lookup:post_heap_lookup
      ~struct_layouts
  in
  Pp.debug
    4
    (lazy
      (item
         "pre memory graph"
         !^(Printf.sprintf
              "%d nodes, %d anchors, %d missing"
              (Memory_graph.Int64Map.cardinal (Memory_graph.info pre_graph))
              (Int64Set.cardinal (Memory_graph.anchors pre_graph))
              (Int64Set.cardinal (Memory_graph.missing pre_graph)))));
  Pp.debug
    4
    (lazy
      (item
         "post memory graph"
         !^(Printf.sprintf
              "%d nodes, %d anchors, %d missing"
              (Memory_graph.Int64Map.cardinal (Memory_graph.info post_graph))
              (Int64Set.cardinal (Memory_graph.anchors post_graph))
              (Int64Set.cardinal (Memory_graph.missing post_graph)))));
  let signature_args =
    Option.value ~default:[] (StdList.assoc_opt func_name function_args)
  in
  let signature_arg_type name = StdList.assoc_opt name signature_args in
  let arg_bt_of_ct (ct : Sctypes.t) : BaseTypes.t =
    match ct with Sctypes.Pointer _ -> BaseTypes.Loc () | _ -> Integer
  in
  let arg_of_var (v : Data_point.var_binding) : Enumerator.arg =
    let sym = Sym.fresh v.name in
    match signature_arg_type v.name with
    | Some (Sctypes.Pointer ((Sctypes.Void | Sctypes.Function _) as _ct)) ->
      { sym; bt = BaseTypes.Loc (); owned_ct = None }
    | Some (Sctypes.Pointer ct) -> { sym; bt = BaseTypes.Loc (); owned_ct = Some ct }
    | Some ct -> { sym; bt = arg_bt_of_ct ct; owned_ct = None }
    | None ->
      let bt : BaseTypes.t =
        if v.size = 8 then
          Loc ()
        else
          Integer
      in
      { sym; bt; owned_ct = None }
  in
  (* Extract function arguments with actual pointee types when available. *)
  let args = StdList.map arg_of_var representative_dp.pre_vars in
  (* Build variable name → address mapping for predicate connectivity check *)
  let var_addrs =
    StdList.map
      (fun (v : Data_point.var_binding) -> (v.name, v.value))
      representative_dp.Data_point.pre_vars
  in
  (* Enumerate, score and cover qualifiers for one phase (pre or post).
     pre and post differ only in which heap graph and must-cover set they use. *)
  let cover_phase ~(phase : string) ~(graph : Memory_graph.t) ~(must_cover : Int64Set.t)
    : Cover.cover_result
    =
    let candidates_raw =
      Enumerator.enumerate ~config ~args ~pred_defs ~graph ~var_addrs ~loc
    in
    Pp.debug
      4
      (lazy
        (item
           (phase ^ " candidates (raw)")
           (Pp.int (StdList.length candidates_raw) ^^^ !^"qualifiers")));
    StdList.iter
      (fun q -> Pp.debug 5 (lazy (item ("  " ^ phase ^ " candidate") (Qualifier.pp q))))
      candidates_raw;
    Pp.debug
      3
      (lazy
        (item
           (phase ^ " must-cover")
           (Pp.int (Int64Set.cardinal must_cover) ^^^ !^"bytes")));
    let candidates =
      StdList.filter_map
        (fun q ->
           match
             Footprint.compute_with_graph q representative_dp graph ~struct_layouts
           with
           | Some fp when not (Int64Set.is_empty (Int64Set.inter fp must_cover)) ->
             let covers = Int64Set.cardinal (Int64Set.inter fp must_cover) in
             Pp.debug
               5
               (lazy
                 (item
                    ("  " ^ phase ^ " footprint")
                    (Qualifier.pp q
                     ^^^ !^"->"
                     ^^^ Pp.int (Int64Set.cardinal fp)
                     ^^^ !^"bytes,"
                     ^^^ Pp.int covers
                     ^^^ !^"covering must")));
             Some { Cover.qualifier = q; footprint = fp }
           | _ -> None)
        candidates_raw
    in
    Pp.debug
      4
      (lazy
        (item
           (phase ^ " candidates with footprints")
           (Pp.int (StdList.length candidates))));
    let result = Cover.cover ~must_cover ~candidates in
    Pp.debug
      3
      (lazy
        (let n_sel = StdList.length result.selected in
         let n_uncov = Int64Set.cardinal result.uncovered in
         item
           (phase ^ " cover result")
           (Pp.int n_sel ^^^ !^"selected," ^^^ Pp.int n_uncov ^^^ !^"uncovered")));
    result
  in
  let pre_result =
    cover_phase ~phase:"pre" ~graph:pre_graph ~must_cover:(pre_must_cover_set dps)
  in
  let post_result =
    cover_phase ~phase:"post" ~graph:post_graph ~must_cover:(post_must_cover_set dps)
  in
  let qualifiers =
    if Int64Set.is_empty pre_result.uncovered && Int64Set.is_empty post_result.uncovered
    then
      Some { pre = pre_result.selected; post = post_result.selected }
    else
      None
  in
  { function_name = func_name; qualifiers }


(** Main entry point: run inference on execution data. *)
let infer
      ~(config : Enumerator.config)
      ~(execution_data : Data_point.execution_data)
      ~(pre_heap_lookup : int64 -> int64 option)
      ~(post_heap_lookup : int64 -> int64 option)
      ~(pred_defs : Definition.Predicate.t Sym.Map.t)
      ~(struct_defs : (Id.t * Sctypes.t) list Sym.Map.t)
      ~(function_args : (string * (string * Sctypes.t) list) list)
  : inferred_spec list
  =
  let open Pp in
  Pp.debug 2 (lazy (headline "bi-abd: starting inference"));
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
  StdList.map
    (fun (func_name, dps) ->
       infer_function
         ~config
         ~pred_defs
         ~struct_defs
         ~function_args
         ~pre_heap_lookup
         ~post_heap_lookup
         ~func_name
         ~dps)
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
  let pre_heap_lookup = Data_point.heap_lookup pre_dumps in
  let post_heap_lookup = Data_point.heap_lookup post_dumps in
  infer
    ~config
    ~execution_data
    ~pre_heap_lookup
    ~post_heap_lookup
    ~pred_defs
    ~struct_defs
    ~function_args

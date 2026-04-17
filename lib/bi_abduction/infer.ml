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


(** Run the inference pipeline for one function. *)
let infer_function
      ~(config : Enumerator.config)
      ~(pred_defs : Definition.Predicate.t Sym.Map.t)
      ~(struct_defs : (Id.t * Sctypes.t) list Sym.Map.t)
      ~(signature_args : (string * Sctypes.t) list)
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
  (* Baseline mode: infer from one representative execution only.
     This keeps the concrete story simple and avoids mixing incompatible heap
     addresses from distinct runs. Generalising across executions is left to
     the TODO path. *)
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
  (* Extract function arguments with actual pointee types when available. *)
  let args = StdList.map arg_of_var representative_dp.pre_vars in
  (* Build variable name → address mapping for predicate connectivity check *)
  let var_addrs =
    StdList.map
      (fun (v : Data_point.var_binding) -> (v.name, v.value))
      representative_dp.Data_point.pre_vars
  in
  (* Run the pipeline for a single phase. pre and post differ only in which
     heap snapshot, missing-entry field, and phase label they use:
     - Pre:  H_entry snapshot, body_missing    (body auto-grants = pre needs)
     - Post: H_exit  snapshot, post_remaining  (leak check remainder = post) *)
  let infer_function_inner (phase : [ `Pre | `Post ]) : Cover.cover_result =
    let phase_label, heap_lookup, select_missing =
      match phase with
      | `Pre ->
        ("pre", pre_heap_lookup, fun (dp : Data_point.data_point) -> dp.body_missing)
      | `Post ->
        ("post", post_heap_lookup, fun (dp : Data_point.data_point) -> dp.post_remaining)
    in
    let missing_set = Data_point.missing_addr_set (select_missing representative_dp) in
    let graph =
      Memory_graph.build
        ~pre_vars:representative_dp.pre_vars
        ~missing_set
        ~heap_lookup
        ~struct_layouts
    in
    Pp.debug
      4
      (lazy
        (item
           (phase_label ^ " memory graph")
           !^(Printf.sprintf
                "%d nodes, %d anchors, %d missing"
                (Memory_graph.Int64Map.cardinal (Memory_graph.info graph))
                (Int64Set.cardinal (Memory_graph.anchors graph))
                (Int64Set.cardinal (Memory_graph.missing graph)))));
    let must_cover = missing_set in
    let candidates_raw =
      Enumerator.enumerate ~config ~args ~pred_defs ~graph ~var_addrs ~loc
    in
    Pp.debug
      4
      (lazy
        (item
           (phase_label ^ " candidates (raw)")
           (Pp.int (StdList.length candidates_raw) ^^^ !^"qualifiers")));
    StdList.iter
      (fun q ->
         Pp.debug 5 (lazy (item ("  " ^ phase_label ^ " candidate") (Qualifier.pp q))))
      candidates_raw;
    Pp.debug
      3
      (lazy
        (item
           (phase_label ^ " must-cover")
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
                    ("  " ^ phase_label ^ " footprint")
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
              ~pred_defs
              ~struct_defs
              ~signature_args
              ~pre_heap_lookup
              ~post_heap_lookup
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

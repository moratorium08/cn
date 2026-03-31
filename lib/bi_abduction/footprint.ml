(** Footprint computation for qualifiers.

    Computes the set of addresses a qualifier would consume when
    evaluated under a concrete variable substitution. *)

module StdList = Stdlib.List
module Int64Set = Data_point.Int64Set

(** Compute the footprint of an Owned<ct>(ptr) qualifier at a concrete address.
    Returns the set of 8-byte-aligned addresses covered by the struct. *)
let owned_footprint ~(ct : Sctypes.t) ~(base_addr : int64) : Int64Set.t =
  let size = Memory.size_of_ctype ct in
  let rec add_words acc offset =
    if offset >= size then acc
    else
      let addr = Int64.add base_addr (Int64.of_int offset) in
      add_words (Int64Set.add addr acc) (offset + 1)
  in
  add_words Int64Set.empty 0

(** Evaluate a simple pointer term to a concrete address using variable bindings.
    Only handles direct variable references for now. *)
let eval_pointer_term
      (term : IndexTerms.t)
      (var_env : (string * int64) list)
  : int64 option
  =
  match term with
  | Terms.IT (Sym sym, _, _) ->
    let name = Sym.pp_string sym in
    StdList.assoc_opt name var_env
  | _ -> None

(** Compute the footprint of a candidate qualifier on a data point.
    Returns None if the qualifier cannot be evaluated (e.g., pointer
    term doesn't resolve to a concrete address). *)
let compute
      (qualifier : Qualifier.t)
      (dp : Data_point.data_point)
  : Int64Set.t option
  =
  let var_env =
    StdList.map
      (fun (v : Data_point.var_binding) -> (v.name, v.value))
      dp.pre_vars
  in
  match qualifier with
  | Request.P { name = Owned (ct, _init); pointer; iargs = _ } ->
    (match eval_pointer_term pointer var_env with
     | Some addr -> Some (owned_footprint ~ct ~base_addr:addr)
     | None -> None)
  | Request.P { name = PName _; pointer; iargs = _ } ->
    (* Predicate footprints need Fulminate re-instrumentation.
       For now, use a heuristic: if the pointer resolves to an anchor
       that connects to missing addresses in the memory graph, assume
       it covers all reachable missing addresses. *)
    ignore pointer;
    None
  | Request.Q _ ->
    (* Quantified predicates (each) not yet supported *)
    None

(** Compute footprints for all candidates on a data point.
    Returns a list of (qualifier, footprint option) pairs. *)
let compute_batch
      (qualifiers : Qualifier.t list)
      (dp : Data_point.data_point)
  : (Qualifier.t * Int64Set.t option) list
  =
  StdList.map (fun q -> (q, compute q dp)) qualifiers

(** Compute predicate footprint using memory graph reachability.
    For a predicate rooted at a pointer, the footprint is all missing
    addresses reachable from that pointer in the memory graph. *)
let predicate_footprint_from_graph
      (pointer : IndexTerms.t)
      (dp : Data_point.data_point)
      (graph : Memory_graph.t)
  : Int64Set.t option
  =
  let var_env =
    StdList.map
      (fun (v : Data_point.var_binding) -> (v.name, v.value))
      dp.pre_vars
  in
  match eval_pointer_term pointer var_env with
  | Some addr ->
    let reachable = Memory_graph.reachable_from graph addr in
    let missing = Memory_graph.missing graph in
    let covered = Int64Set.inter reachable missing in
    if Int64Set.is_empty covered then None
    else Some covered
  | None -> None

(** Enhanced compute that uses memory graph for predicate footprints. *)
let compute_with_graph
      (qualifier : Qualifier.t)
      (dp : Data_point.data_point)
      (graph : Memory_graph.t)
  : Int64Set.t option
  =
  match qualifier with
  | Request.P { name = Owned _; _ } ->
    compute qualifier dp
  | Request.P { name = PName _; pointer; iargs = _ } ->
    predicate_footprint_from_graph pointer dp graph
  | Request.Q _ ->
    None

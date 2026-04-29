(** Footprint computation for qualifiers.

    Owned qualifiers resolve to a contiguous byte range purely from the
    pointer's value in the data point.  Predicate qualifiers are computed by
    a generated C harness (see [Fp_codegen] / [Fp_runner]); this module
    intentionally does not know how to compute them on its own. *)

module StdList = Stdlib.List
module Int64Set = Data_point.Int64Set

(** Compute the footprint of an Owned<ct>(ptr) qualifier at a concrete address. *)
let owned_footprint ~(ct : Sctypes.t) ~(base_addr : int64) : Int64Set.t =
  let size = Memory.size_of_ctype ct in
  let rec add_words acc offset =
    if offset >= size then
      acc
    else (
      let addr = Int64.add base_addr (Int64.of_int offset) in
      add_words (Int64Set.add addr acc) (offset + 1))
  in
  add_words Int64Set.empty 0


(** Evaluate a simple pointer term to a concrete address using variable bindings. *)
let eval_pointer_term (term : IndexTerms.t) (var_env : (string * int64) list)
  : int64 option
  =
  match term with
  | Terms.IT (Sym sym, _, _) ->
    let name = Sym.pp_string sym in
    StdList.assoc_opt name var_env
  | _ -> None


(** Compute the footprint of an [Owned] qualifier on a data point.  Predicate
    qualifiers always return [None] from this entry point; their footprints
    come from the C harness. *)
let compute (qualifier : Qualifier.t) (dp : Data_point.data_point) : Int64Set.t option =
  let var_env =
    StdList.map (fun (v : Data_point.var_binding) -> (v.name, v.value)) dp.pre_vars
  in
  match qualifier with
  | Request.P { name = Owned (ct, _init); pointer; iargs = _ } ->
    (match eval_pointer_term pointer var_env with
     | Some addr -> Some (owned_footprint ~ct ~base_addr:addr)
     | None -> None)
  | Request.P { name = PName _; _ } -> None
  | Request.Q _ -> None


let compute_batch (qualifiers : Qualifier.t list) (dp : Data_point.data_point)
  : (Qualifier.t * Int64Set.t option) list
  =
  StdList.map (fun q -> (q, compute q dp)) qualifiers

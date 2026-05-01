(** Footprint computation for qualifiers.

    Owned qualifiers resolve to a contiguous byte range purely from the
    pointer's value in the data point.  Predicate qualifiers are computed by
    a generated C harness (see [Fp_codegen] / [Fp_runner]) that executes
    their Fulminate semantics against recorded heap snapshots. *)

module StdList = Stdlib.List
module CF = Cerb_frontend
module Int64Set = Data_point.Int64Set

type harness_ctx =
  { cc : string;
    output_dir : string;
    cn_runtime_prefix : string;
    filename : string;
    cabs_tunit : CF.Cabs.translation_unit;
    ail_prog : CF.GenTypes.genTypeCategory CF.AilSyntax.sigma;
    prog5 : unit Mucore.file
  }

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


(** Evaluate a simple pointer term to a concrete address using pre-state variables. *)
let eval_pointer_term
      (term : IndexTerms.t)
      (pre_vars : Data_point.var_binding list)
  : int64 option
  =
  match term with
  | Terms.IT (Sym sym, _, _) ->
    let name = Sym.pp_string sym in
    StdList.find_map
      (fun (v : Data_point.var_binding) ->
         if String.equal v.name name then Some v.value else None)
      pre_vars
  | _ -> None


(** Compute the footprint of an [Owned] qualifier on a data point.  Predicate
    qualifiers always return [None] from this entry point; their footprints
    come from the C harness. *)
let compute (qualifier : Qualifier.t) (dp : Data_point.data_point) : Int64Set.t option =
  match qualifier with
  | Request.P { name = Owned (ct, _init); pointer; iargs = _ } ->
    (match eval_pointer_term pointer dp.pre_vars with
     | Some addr -> Some (owned_footprint ~ct ~base_addr:addr)
     | None -> None)
  | Request.P { name = PName _; _ } -> None
  | Request.Q _ -> None


let compute_batch (qualifiers : Qualifier.t list) (dp : Data_point.data_point)
  : (Qualifier.t * Int64Set.t option) list
  =
  StdList.map (fun q -> (q, compute q dp)) qualifiers


let compute_predicate_table
      ~(harness : harness_ctx)
      ~(tag : string)
      ~(func_name : string)
      ~(pred_defs : Definition.Predicate.t Sym.Map.t)
      ~(data_points : Fp_codegen.dp_entry list)
      ~(qualifiers : (int * Qualifier.t) list)
  : Fp_table.t
  =
  match qualifiers with
  | [] -> Fp_table.empty
  | _ ->
    let codegen_input : Fp_codegen.input =
      { filename = harness.filename;
        cabs_tunit = harness.cabs_tunit;
        ail_prog = harness.ail_prog;
        prog5 = harness.prog5;
        pred_defs;
        data_points;
        qualifiers;
        output_json_path = "" (* set inside Fp_runner.run *)
      }
    in
    Fp_runner.run
      ~cc:harness.cc
      ~output_dir:harness.output_dir
      ~cn_runtime_prefix:harness.cn_runtime_prefix
      ~func_name
      ~tag
      codegen_input


let lookup
      ~(representative_dp : Data_point.data_point)
      ~(representative_dp_idx : int)
      ~(fp_table : Fp_table.t)
      ((q_idx, q) : int * Qualifier.t)
  : Int64Set.t option
  =
  match q with
  | Request.P { name = Owned _; _ } -> compute q representative_dp
  | Request.P { name = PName _; _ } ->
    (match Fp_table.find fp_table (q_idx, representative_dp_idx) with
     | Some fp -> fp
     | None -> None)
  | _ -> None

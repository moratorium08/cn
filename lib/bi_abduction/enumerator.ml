(** Bounded enumeration of candidate qualifiers.

    Given function arguments, struct definitions, predicate definitions,
    and observed data points, generate a bounded set of candidate qualifiers
    to try during coverage analysis. *)

module StdList = Stdlib.List
module IT = IndexTerms
module BT = IndexTerms.BT

type arg =
  { sym : Sym.t;
    bt : BaseTypes.t;
    owned_ct : Sctypes.t option
  }

type config = { max_qualifiers : int }

let default_config = { max_qualifiers = 1000 }

(** Convert surface BaseTypes.t to internal IndexTerms.BT.t by erasing Loc info. *)
let rec bt_to_internal : BaseTypes.t -> BT.t = function
  | BaseTypes.Unit -> BT.Unit
  | BaseTypes.Bool -> BT.Bool
  | BaseTypes.Integer -> BT.Integer
  | BaseTypes.MemByte -> BT.MemByte
  | BaseTypes.Bits (sign, n) -> BT.Bits (sign, n)
  | BaseTypes.Real -> BT.Real
  | BaseTypes.Alloc_id -> BT.Alloc_id
  | BaseTypes.Loc _ -> BT.Loc ()
  | BaseTypes.CType -> BT.CType
  | BaseTypes.Struct s -> BT.Struct s
  | BaseTypes.Datatype s -> BT.Datatype s
  | BaseTypes.Record members ->
    BT.Record (StdList.map (fun (id, bt) -> (id, bt_to_internal bt)) members)
  | BaseTypes.Map (k, v) -> BT.Map (bt_to_internal k, bt_to_internal v)
  | BaseTypes.List bt -> BT.List (bt_to_internal bt)
  | BaseTypes.Tuple bts -> BT.Tuple (StdList.map bt_to_internal bts)
  | BaseTypes.Set bt -> BT.Set (bt_to_internal bt)
  | BaseTypes.Option bt -> BT.Option (bt_to_internal bt)


(** Generate base pointer terms from function arguments. *)
let base_pointer_terms (args : arg list) (loc : Locations.t) : IT.t list =
  args
  |> StdList.filter_map (fun (arg : arg) ->
    match arg.bt with
    | BaseTypes.Loc _ -> Some (IT.sym_ (arg.sym, bt_to_internal arg.bt, loc))
    | _ -> None)


type term_choice =
  { term : IT.t;
    arg_sym : Sym.t option
  }


let arg_choice ~(loc : Locations.t) (arg : arg) : term_choice =
  { term = IT.sym_ (arg.sym, bt_to_internal arg.bt, loc); arg_sym = Some arg.sym }


let bits_constants (sign, sz) loc : term_choice list =
  [ 0; 1; -1 ]
  |> StdList.filter_map (fun n ->
    let z = Z.of_int n in
    if BT.fits_range (sign, sz) z then
      Some { term = IT.num_lit_ z (BT.Bits (sign, sz)) loc; arg_sym = None }
    else
      None)


(** Simple literal choices used for predicate iargs.  These are deliberately
    small: the naive part is in trying every well-typed placement, not in
    inventing arbitrary literal values. *)
let constant_choices_for_bt ~(loc : Locations.t) (bt : BaseTypes.t) : term_choice list =
  match bt with
  | BaseTypes.Unit -> [ { term = IT.unit_ loc; arg_sym = None } ]
  | BaseTypes.Bool ->
    [ { term = IT.bool_ true loc; arg_sym = None };
      { term = IT.bool_ false loc; arg_sym = None }
    ]
  | BaseTypes.Integer ->
    [ { term = IT.int_ 0 loc; arg_sym = None };
      { term = IT.int_ 1 loc; arg_sym = None };
      { term = IT.int_ (-1) loc; arg_sym = None }
    ]
  | BaseTypes.Bits (sign, sz) -> bits_constants (sign, sz) loc
  | BaseTypes.Real ->
    [ { term = IT.q_ (0, 1) loc; arg_sym = None };
      { term = IT.q_ (1, 1) loc; arg_sym = None }
    ]
  | BaseTypes.Loc _ -> [ { term = IT.null_ loc; arg_sym = None } ]
  | _ -> []


(** Generate all terms matching a requested base type from in-scope arguments
    and small constants. *)
let choices_for_bt ~(args : arg list) ~(loc : Locations.t) ~(bt : BaseTypes.t)
  : term_choice list
  =
  let arg_choices =
    args
    |> StdList.filter (fun (arg : arg) -> BaseTypes.equal arg.bt bt)
    |> StdList.map (arg_choice ~loc)
  in
  arg_choices @ constant_choices_for_bt ~loc bt

(** Generate Owned qualifiers from the actual pointee type of each pointer
    argument. *)
let owned_qualifiers ~(args : arg list) ~(loc : Locations.t) : Qualifier.t list =
  StdList.filter_map
    (fun (arg : arg) ->
       match arg.owned_ct with
       | None -> None
       | Some ct ->
         let ptr_term = IT.sym_ (arg.sym, bt_to_internal arg.bt, loc) in
         Some (Qualifier.owned ~ct ~pointer:ptr_term))
    args


let dedup (qs : Qualifier.t list) : Qualifier.t list =
  StdList.rev
    (StdList.fold_left
       (fun acc q ->
          if StdList.exists (Qualifier.equal q) acc then acc else q :: acc)
       []
       qs)


(** Generate predicate qualifiers naively.

    This intentionally avoids heap-shape and predicate-body heuristics.  For
    each predicate and each pointer argument as root, enumerate every well-typed
    assignment of predicate iargs from function arguments plus simple constants.
    Function arguments are used as a typed permutation: the same argument symbol
    is not reused within one predicate application. *)
let predicate_qualifiers
      ~(args : arg list)
      ~(pred_defs : Definition.Predicate.t Sym.Map.t)
      ~(loc : Locations.t)
  : Qualifier.t list
  =
  let root_choices =
    args
    |> StdList.filter (fun (arg : arg) ->
      match arg.bt with BaseTypes.Loc _ -> true | _ -> false)
    |> StdList.map (arg_choice ~loc)
  in
  let arg_already_used used = function
    | None -> false
    | Some sym -> StdList.exists (Sym.equal sym) used
  in
  let rec choose_iargs used = function
    | [] -> [ [] ]
    | (_iarg_sym, iarg_bt) :: rest ->
      let choices =
        choices_for_bt ~args ~loc ~bt:iarg_bt
        |> StdList.filter (fun choice -> not (arg_already_used used choice.arg_sym))
      in
      StdList.concat_map
        (fun choice ->
           let used =
             match choice.arg_sym with None -> used | Some sym -> sym :: used
           in
           StdList.map
             (fun suffix -> choice.term :: suffix)
             (choose_iargs used rest))
        choices
  in
  Sym.Map.fold
    (fun pred_name (pred_def : Definition.Predicate.t) acc ->
       let new_qs =
         StdList.concat_map
           (fun root ->
              let used =
                match root.arg_sym with None -> [] | Some sym -> [ sym ]
              in
              choose_iargs used pred_def.iargs
              |> StdList.map (fun iargs ->
                Qualifier.predicate ~name:pred_name ~pointer:root.term ~iargs))
           root_choices
       in
       new_qs @ acc)
    pred_defs
    []


(* HK(TODO): make this lazy *)

(** Main enumeration entry point. *)
let enumerate
      ~(config : config)
      ~(args : arg list)
      ~(pred_defs : Definition.Predicate.t Sym.Map.t)
      ~(graph : Memory_graph.t)
      ~(var_addrs : (string * int64) list)
      ~(loc : Locations.t)
  : Qualifier.t list
  =
  ignore config;
  ignore graph;
  ignore var_addrs;
  let owned_qs = owned_qualifiers ~args ~loc in
  Pp.debug 4 (lazy (Pp.item "enum: owned qualifiers" (Pp.int (StdList.length owned_qs))));
  let pred_qs = predicate_qualifiers ~args ~pred_defs ~loc in
  Pp.debug
    4
    (lazy (Pp.item "enum: predicate qualifiers" (Pp.int (StdList.length pred_qs))));
  dedup (owned_qs @ pred_qs)

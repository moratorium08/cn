(** Bounded enumeration of candidate qualifiers.

    Given function arguments, struct definitions, predicate definitions,
    and observed data points, generate a bounded set of candidate qualifiers
    to try during coverage analysis. *)

module StdList = Stdlib.List

module Int64Set = Data_point.Int64Set
module IT = IndexTerms
module BT = IndexTerms.BT

type config =
  { max_term_depth : int;
    max_qualifiers : int;
    max_chain_depth : int;
    max_pred_unfolding : int
  }

let default_config =
  { max_term_depth = 3;
    max_qualifiers = 1000;
    max_chain_depth = 3;
    max_pred_unfolding = 100
  }

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
let base_pointer_terms
      (args : (Sym.t * BaseTypes.t) list)
      (loc : Locations.t)
  : IT.t list
  =
  args
  |> StdList.filter_map (fun (sym, bt) ->
    match bt with
    | BaseTypes.Loc _ -> Some (IT.sym_ (sym, bt_to_internal bt, loc))
    | _ -> None)

(** Generate in-scope variable terms matching a requested base type.
    For predicate iargs, prefer terms other than the root pointer first so that
    segment-like predicates choose a distinct boundary variable when available. *)
let arg_terms_for_bt
      ~(args : (Sym.t * BaseTypes.t) list)
      ~(loc : Locations.t)
      ~(preferred_not : Sym.t option)
      ~(bt : BaseTypes.t)
  : IT.t list
  =
  let matching =
    StdList.filter (fun (_sym, arg_bt) -> BaseTypes.equal arg_bt bt) args
  in
  let matching =
    match preferred_not with
    | None -> matching
    | Some avoid_sym ->
      let preferred, fallback =
        StdList.partition (fun (sym, _bt) -> not (Sym.equal sym avoid_sym)) matching
      in
      preferred @ fallback
  in
  StdList.map
    (fun (sym, bt) -> IT.sym_ (sym, bt_to_internal bt, loc))
    matching

(** Cartesian product of choice lists. *)
let rec combinations : 'a list list -> 'a list list = function
  | [] -> [ [] ]
  | xs :: rest ->
    let rest_combos = combinations rest in
    StdList.concat_map
      (fun x -> StdList.map (fun suffix -> x :: suffix) rest_combos)
      xs

(** Generate Owned qualifiers for each pointer argument and each struct type. *)
let owned_qualifiers
      ~(args : (Sym.t * BaseTypes.t) list)
      ~(struct_defs : (Id.t * Sctypes.t) list Sym.Map.t)
      ~(loc : Locations.t)
  : Qualifier.t list
  =
  let ptr_terms = base_pointer_terms args loc in
  let struct_types =
    Sym.Map.fold (fun tag _fields acc -> Sctypes.Struct tag :: acc)
      struct_defs []
  in
  StdList.concat_map
    (fun ptr_term ->
       StdList.map
         (fun ct -> Qualifier.owned ~ct ~pointer:ptr_term)
         struct_types)
    ptr_terms

(** Extract access paths from a recursive predicate definition.
    Returns the field names that the predicate traverses when unfolding. *)
let traversal_fields
      (pred_def : Definition.Predicate.t)
      ~(struct_defs : (Id.t * Sctypes.t) list Sym.Map.t)
  : Id.t list list
  =
  ignore struct_defs;
  match pred_def.clauses with
  | None -> []
  | Some clauses ->
    StdList.filter_map
      (fun (clause : Definition.Clause.t) ->
         ignore clause;
         (* TODO: Walk LogicalArgumentTypes to extract traversal fields.
            For now, predicates are matched via memory graph connectivity. *)
         None)
      clauses

(** Generate predicate qualifiers by checking if a predicate's traversal
    pattern matches the memory graph structure. *)
let predicate_qualifiers
      ~(config : config)
      ~(args : (Sym.t * BaseTypes.t) list)
      ~(pred_defs : Definition.Predicate.t Sym.Map.t)
      ~(graph : Memory_graph.t)
      ~(var_addrs : (string * int64) list)
      ~(loc : Locations.t)
  : Qualifier.t list
  =
  ignore config;
  let ptr_terms = base_pointer_terms args loc in
  StdList.concat_map
    (fun ptr_term ->
       match IT.is_sym ptr_term with
       | None -> []
       | Some (sym, _bt) ->
         (* Look up this pointer's concrete address *)
         let sym_name = Sym.pp_string sym in
         let sym_addr = StdList.assoc_opt sym_name var_addrs in
         Sym.Map.fold
           (fun pred_name pred_def acc ->
              if not pred_def.Definition.Predicate.recursive then acc
              else begin
                (* Check if THIS pointer connects to missing addresses *)
                let connects =
                  match sym_addr with
                  | Some addr -> Memory_graph.connects_to_missing graph addr
                  | None ->
                    (* Fallback: check all anchors *)
                    let anchors = Memory_graph.anchors graph in
                    Int64Set.exists
                      (fun a -> Memory_graph.connects_to_missing graph a)
                      anchors
                in
                Pp.debug 5 (lazy (Pp.string
                  (Printf.sprintf "enum: pred %s(%s) connects=%b"
                     (Sym.pp_string pred_name) sym_name connects)));
                if connects then begin
                  let iarg_choices =
                    StdList.map
                      (fun (_iarg_sym, iarg_bt) ->
                         arg_terms_for_bt ~args ~loc ~preferred_not:(Some sym) ~bt:iarg_bt)
                      pred_def.iargs
                  in
                  if StdList.exists (function [] -> true | _ -> false) iarg_choices then
                    acc
                  else
                    let iarg_combos = combinations iarg_choices in
                    let new_qs =
                      StdList.map
                        (fun iargs ->
                           Qualifier.predicate
                             ~name:pred_name
                             ~pointer:ptr_term
                             ~iargs)
                        iarg_combos
                    in
                    new_qs @ acc
                end else
                  acc
              end)
           pred_defs [])
    ptr_terms

(** Main enumeration entry point. *)
let enumerate
      ~(config : config)
      ~(args : (Sym.t * BaseTypes.t) list)
      ~(pred_defs : Definition.Predicate.t Sym.Map.t)
      ~(struct_defs : (Id.t * Sctypes.t) list Sym.Map.t)
      ~(graph : Memory_graph.t)
      ~(var_addrs : (string * int64) list)
      ~(loc : Locations.t)
  : Qualifier.t list
  =
  let owned_qs = owned_qualifiers ~args ~struct_defs ~loc in
  Pp.debug 4 (lazy (Pp.item "enum: owned qualifiers" (Pp.int (StdList.length owned_qs))));
  let pred_qs =
    predicate_qualifiers ~config ~args ~pred_defs ~graph ~var_addrs ~loc
  in
  Pp.debug 4 (lazy (Pp.item "enum: predicate qualifiers" (Pp.int (StdList.length pred_qs))));
  let all = owned_qs @ pred_qs in
  if StdList.length all > config.max_qualifiers then begin
    Pp.debug 3 (lazy (Pp.string
      (Printf.sprintf "enum: truncating %d candidates to %d"
         (StdList.length all) config.max_qualifiers)));
    StdList.filteri (fun i _ -> i < config.max_qualifiers) all
  end else
    all

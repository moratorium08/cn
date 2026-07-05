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


let arg_term ~(loc : Locations.t) (arg : arg) : IT.t =
  IT.sym_ (arg.sym, bt_to_internal arg.bt, loc)


let bits_constants (sign, sz) loc : IT.t list =
  [ 0; 1; -1 ]
  |> StdList.filter_map (fun n ->
    let z = Z.of_int n in
    if BT.fits_range (sign, sz) z then
      Some (IT.num_lit_ z (BT.Bits (sign, sz)) loc)
    else
      None)


(** Simple literal choices used for predicate iargs.  These are deliberately
    small: the naive part is in trying every well-typed placement, not in
    inventing arbitrary literal values. *)
let constant_choices_for_bt ~(loc : Locations.t) (bt : BaseTypes.t) : IT.t list =
  match bt with
  | BaseTypes.Unit -> [ IT.unit_ loc ]
  | BaseTypes.Bool -> [ IT.bool_ true loc; IT.bool_ false loc ]
  | BaseTypes.Integer -> [ IT.int_ 0 loc; IT.int_ 1 loc; IT.int_ (-1) loc ]
  | BaseTypes.Bits (sign, sz) -> bits_constants (sign, sz) loc
  | BaseTypes.Real -> [ IT.q_ (0, 1) loc; IT.q_ (1, 1) loc ]
  | BaseTypes.Loc _ -> [ IT.null_ loc ]
  | _ -> []


(** Generate all terms matching a requested base type from in-scope arguments
    and small constants. *)
let choices_for_bt ~(args : arg list) ~(loc : Locations.t) ~(bt : BaseTypes.t) : IT.t list
  =
  let arg_choices =
    args
    |> StdList.filter (fun (arg : arg) -> BaseTypes.equal arg.bt bt)
    |> StdList.map (arg_term ~loc)
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
       (fun acc q -> if StdList.exists (Qualifier.equal q) acc then acc else q :: acc)
       []
       qs)


(** Generate predicate qualifiers naively.

    This intentionally avoids heap-shape and predicate-body heuristics.  For
    each predicate and each pointer argument as root, enumerate every well-typed
    assignment of predicate iargs from function arguments plus simple constants. *)
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
    |> StdList.map (arg_term ~loc)
  in
  let rec choose_iargs = function
    | [] -> [ [] ]
    | (_iarg_sym, iarg_bt) :: rest ->
      let choices = choices_for_bt ~args ~loc ~bt:iarg_bt in
      StdList.concat_map
        (fun choice -> StdList.map (fun suffix -> choice :: suffix) (choose_iargs rest))
        choices
  in
  Sym.Map.fold
    (fun pred_name (pred_def : Definition.Predicate.t) acc ->
       let new_qs =
         StdList.concat_map
           (fun root ->
              choose_iargs pred_def.iargs
              |> StdList.map (fun iargs ->
                Qualifier.predicate ~name:pred_name ~pointer:root ~iargs))
           root_choices
       in
       new_qs @ acc)
    pred_defs
    []


(** Generate depth-2 qualifier chains (IDEA.md 4.4):

      take W = RW<struct S>(arg); take _ = Q(W.field);

    for each struct-pointer argument, each pointer-typed field of the
    pointee, and each leaf request Q (a user predicate with naively chosen
    iargs, or RW of the field's pointee type).  The prefix step is built
    *once per argument* with a stable bound name, so chains rooted at the
    same argument share it — Cover and the printer merge shared prefixes
    (the duplicated-prefix problem of IDEA.md 4.4). *)
let chain_qualifiers
      ~(args : arg list)
      ~(pred_defs : Definition.Predicate.t Sym.Map.t)
      ~(struct_defs : (Id.t * Sctypes.t) list Sym.Map.t)
      ~(loc : Locations.t)
  : Qualifier.t list
  =
  let rec choose_iargs = function
    | [] -> [ [] ]
    | (_iarg_sym, iarg_bt) :: rest ->
      let choices = choices_for_bt ~args ~loc ~bt:iarg_bt in
      StdList.concat_map
        (fun choice -> StdList.map (fun suffix -> choice :: suffix) (choose_iargs rest))
        choices
  in
  StdList.concat_map
    (fun (arg : arg) ->
       match arg.owned_ct with
       | Some (Sctypes.Struct tag as struct_ct) ->
         (match Sym.Map.find_opt tag struct_defs with
          | None -> []
          | Some fields ->
            let w_sym = Sym.fresh (Sym.pp_string arg.sym ^ "_W") in
            let prefix : Qualifier.step =
              { name = w_sym;
                req =
                  Request.P
                    { name = Owned (struct_ct, Init);
                      pointer = arg_term ~loc arg;
                      iargs = []
                    }
              }
            in
            let w_term = IT.sym_ (w_sym, BT.Struct tag, loc) in
            StdList.concat_map
              (fun (fid, field_ct) ->
                 match field_ct with
                 | Sctypes.Pointer ((Sctypes.Void | Sctypes.Function _) as _unsupported)
                   ->
                   []
                 | Sctypes.Pointer pointee_ct ->
                   let fld_term = IT.member_ ~member_bt:(BT.Loc ()) (w_term, fid) loc in
                   let leaf_owned : Qualifier.step =
                     { name = Sym.fresh_anon ();
                       req =
                         Request.P
                           { name = Owned (pointee_ct, Init);
                             pointer = fld_term;
                             iargs = []
                           }
                     }
                   in
                   let leaf_preds =
                     Sym.Map.fold
                       (fun pred_name (pred_def : Definition.Predicate.t) acc ->
                          let qs =
                            choose_iargs pred_def.iargs
                            |> StdList.map (fun iargs : Qualifier.step ->
                              { name = Sym.fresh_anon ();
                                req =
                                  Request.P
                                    { name = PName pred_name; pointer = fld_term; iargs }
                              })
                          in
                          qs @ acc)
                       pred_defs
                       []
                   in
                   (* predicates first: on footprint ties (e.g. a one-node
                      list) the greedy cover then prefers the
                      shape-generalising predicate over a fixed RW *)
                   StdList.map (fun leaf -> [ prefix; leaf ]) (leaf_preds @ [ leaf_owned ])
                 | _ -> [])
              fields)
       | _ -> [])
    args


(* HK(TODO): make this lazy *)

(** Main enumeration entry point.  Chains come last so that, on footprint
    ties, the greedy cover prefers flat qualifiers. *)
let enumerate
      ~(config : config)
      ~(args : arg list)
      ~(pred_defs : Definition.Predicate.t Sym.Map.t)
      ~(struct_defs : (Id.t * Sctypes.t) list Sym.Map.t)
      ~(loc : Locations.t)
  : Qualifier.t list
  =
  ignore config;
  let owned_qs = owned_qualifiers ~args ~loc in
  Pp.debug 4 (lazy (Pp.item "enum: owned qualifiers" (Pp.int (StdList.length owned_qs))));
  let pred_qs = predicate_qualifiers ~args ~pred_defs ~loc in
  Pp.debug
    4
    (lazy (Pp.item "enum: predicate qualifiers" (Pp.int (StdList.length pred_qs))));
  let chain_qs = chain_qualifiers ~args ~pred_defs ~struct_defs ~loc in
  Pp.debug 4 (lazy (Pp.item "enum: chain qualifiers" (Pp.int (StdList.length chain_qs))));
  dedup (owned_qs @ pred_qs @ chain_qs)

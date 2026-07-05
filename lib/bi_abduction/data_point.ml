(** Parsing and representation of bi-abductive execution data.
    Reads the JSON summary file (cn_abd_summary.json) and JSONL heap dump
    (cn_abd_heap.jsonl) produced by the bi-abductive runtime.

    Wire schema (one entry per activation, keyed by [dp]):
    - summary: {"data_points":[{"dp":N,"function":f,
        "pre":{"vars":[...],"owned":[{addr,size}...]},
        "body":{"missing":[...]},
        "post":{"vars":[...],"remaining":[...]}}]}
    - heap JSONL: {"dp":N,"phase":"pre"|"post","words":{addr:val,...}} *)

(* Restore standard library modules shadowed by CN's library *)
module StdList = Stdlib.List
module String = Stdlib.String
module IntMap = Map.Make (Int)

type var_binding =
  { name : string;
    value : int64;
    size : int
  }

type missing_entry =
  { addr : int64;
    size : int
  }

type data_point =
  { dp_idx : int; (** activation id, in call order *)
    function_name : string;
    pre_vars : var_binding list;
    post_vars : var_binding list; (** post-state bindings, e.g. [return] *)
    owned_pre : missing_entry list;
      (** ownership held after evaluating the user precondition (user
            footprint + parameter/local cells); the complement of the
            sandwich upper bound B *)
    body_missing : missing_entry list; (** materialised anti-frame A *)
    post_remaining : missing_entry list (** leak set Lambda = frame L *)
  }

type execution_data = { data_points : data_point list }

type heap_word =
  { addr : int64;
    value : int64
  }

type heap_dump = { words : heap_word list }

(* --- Parsing helpers --- *)

let parse_hex_int64 s =
  (* Accept "0x..." or plain decimal *)
  if String.length s > 2 && String.equal (String.sub s 0 2) "0x" then
    Int64.of_string s
  else
    Int64.of_string s


let json_string = function
  | `String s -> s
  | j -> failwith ("expected JSON string, got: " ^ Yojson.Safe.to_string j)


let json_int = function
  | `Int n -> n
  | `Intlit s -> int_of_string s
  | j -> failwith ("expected JSON int, got: " ^ Yojson.Safe.to_string j)


let json_list = function
  | `List l -> l
  | j -> failwith ("expected JSON list, got: " ^ Yojson.Safe.to_string j)


let json_assoc = function
  | `Assoc a -> a
  | j -> failwith ("expected JSON object, got: " ^ Yojson.Safe.to_string j)


let json_field key obj =
  match StdList.assoc_opt key (json_assoc obj) with
  | Some v -> v
  | None -> failwith ("missing JSON field: " ^ key)


(* --- Var binding parsing --- *)

let parse_var_binding j =
  { name = json_string (json_field "name" j);
    value = parse_hex_int64 (json_string (json_field "value" j));
    size = json_int (json_field "size" j)
  }


(* --- Missing entry parsing --- *)

let parse_missing_entry j =
  { addr = parse_hex_int64 (json_string (json_field "addr" j));
    size = json_int (json_field "size" j)
  }


let parse_entry_list j = json_list j |> StdList.map parse_missing_entry

(* --- Data point parsing --- *)

let parse_data_point j =
  let pre_obj = json_field "pre" j in
  let post_obj = json_field "post" j in
  { dp_idx = json_int (json_field "dp" j);
    function_name = json_string (json_field "function" j);
    pre_vars = json_list (json_field "vars" pre_obj) |> StdList.map parse_var_binding;
    post_vars = json_list (json_field "vars" post_obj) |> StdList.map parse_var_binding;
    owned_pre = parse_entry_list (json_field "owned" pre_obj);
    body_missing = parse_entry_list (json_field "missing" (json_field "body" j));
    post_remaining = parse_entry_list (json_field "remaining" post_obj)
  }


(* --- Summary file parsing --- *)

let parse_summary_json (filename : string) : execution_data =
  let json = Yojson.Safe.from_file filename in
  { data_points =
      json_list (json_field "data_points" json) |> StdList.map parse_data_point
  }


(* --- Heap dump parsing (JSONL) --- *)

let parse_heap_words_obj j =
  json_assoc j
  |> StdList.map (fun (addr_s, val_j) ->
    { addr = parse_hex_int64 addr_s; value = parse_hex_int64 (json_string val_j) })


let parse_heap_dump_line (line : string) : int * [ `Pre | `Post ] * heap_dump =
  let j = Yojson.Safe.from_string line in
  let dp_idx = json_int (json_field "dp" j) in
  let phase =
    match StdList.assoc_opt "phase" (json_assoc j) with
    | Some (`String "post") -> `Post
    | _ -> `Pre
  in
  let dump = { words = parse_heap_words_obj (json_field "words" j) } in
  (dp_idx, phase, dump)


(** Per-activation heap snapshots: dp index -> dumps for that activation. *)
type heap_dumps_by_dp = heap_dump list IntMap.t

(** Parse a heap JSONL file; returns (pre, post) snapshots keyed by dp. *)
let parse_heap_jsonl (filename : string) : heap_dumps_by_dp * heap_dumps_by_dp =
  let add dp_idx dump m =
    IntMap.update
      dp_idx
      (function Some ds -> Some (dump :: ds) | None -> Some [ dump ])
      m
  in
  let ic = open_in filename in
  let rec read_lines pre post =
    match input_line ic with
    | line ->
      let line = String.trim line in
      if String.length line = 0 then
        read_lines pre post
      else (
        let dp_idx, phase, dump = parse_heap_dump_line line in
        match phase with
        | `Pre -> read_lines (add dp_idx dump pre) post
        | `Post -> read_lines pre (add dp_idx dump post))
    | exception End_of_file -> (pre, post)
  in
  Fun.protect
    ~finally:(fun () -> close_in ic)
    (fun () -> read_lines IntMap.empty IntMap.empty)


(* --- Grouping --- *)

let group_by_function (dps : data_point list) : (string * data_point list) list =
  let tbl : (string, data_point list) Hashtbl.t = Hashtbl.create 16 in
  StdList.iter
    (fun (dp : data_point) ->
       let existing : data_point list =
         match Hashtbl.find_opt tbl dp.function_name with Some l -> l | None -> []
       in
       Hashtbl.replace tbl dp.function_name (dp :: existing))
    dps;
  Hashtbl.fold
    (fun name (points : data_point list) acc -> (name, StdList.rev points) :: acc)
    tbl
    []


(* --- Address set helpers --- *)

module Int64Set = Set.Make (Int64)

let missing_addr_set (entries : missing_entry list) : Int64Set.t =
  StdList.fold_left
    (fun acc (e : missing_entry) ->
       let rec add_range set offset =
         if offset >= e.size then
           set
         else (
           let a = Int64.add e.addr (Int64.of_int offset) in
           add_range (Int64Set.add a set) (offset + 1))
       in
       add_range acc 0)
    Int64Set.empty
    entries


(** Flatten one activation's heap dumps into a deduplicated [(addr, value)]
    list, for feeding to the footprint harness. *)
let heap_words_for_dp (dumps_by_dp : heap_dumps_by_dp) (dp_idx : int)
  : (int64 * int64) list
  =
  match IntMap.find_opt dp_idx dumps_by_dp with
  | None -> []
  | Some dumps ->
    let tbl = Hashtbl.create 256 in
    StdList.iter
      (fun dump -> StdList.iter (fun w -> Hashtbl.replace tbl w.addr w.value) dump.words)
      dumps;
    Hashtbl.fold (fun a v acc -> (a, v) :: acc) tbl []

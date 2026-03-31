(** Parsing and representation of bi-abductive execution data.
    Reads the JSON summary file (.abd.json) and JSONL heap dump (.heap.jsonl)
    produced by the bi-abductive runtime. *)

(* Restore standard library modules shadowed by CN's library *)
module StdList = Stdlib.List
module String = Stdlib.String

type var_binding =
  { name : string;
    value : int64;
    size : int;
    type_name : string
  }

type missing_entry =
  { addr : int64;
    size : int
  }

type data_point =
  { function_name : string;
    pre_vars : var_binding list;
    pre_missing : missing_entry list;
    post_vars : var_binding list;
    post_missing : missing_entry list
  }

type execution_data =
  { version : int;
    data_points : data_point list
  }

type heap_word =
  { addr : int64;
    value : int64
  }

type heap_dump =
  { function_name : string;
    target_addr : int64;
    words : heap_word list
  }

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
    size = json_int (json_field "size" j);
    type_name = json_string (json_field "type" j)
  }

(* --- Missing entry parsing --- *)

let parse_missing_entry j =
  { addr = parse_hex_int64 (json_string (json_field "addr" j));
    size = json_int (json_field "size" j)
  }

(* --- Data point parsing --- *)

let parse_pre_post j =
  let vars =
    json_list (json_field "vars" j) |> StdList.map parse_var_binding
  in
  let missing =
    json_list (json_field "missing" j) |> StdList.map parse_missing_entry
  in
  (vars, missing)

let parse_data_point j =
  let pre_vars, pre_missing = parse_pre_post (json_field "pre" j) in
  let post_vars, post_missing = parse_pre_post (json_field "post" j) in
  { function_name = json_string (json_field "function" j);
    pre_vars;
    pre_missing;
    post_vars;
    post_missing
  }

(* --- Summary file parsing --- *)

let parse_summary_json (filename : string) : execution_data =
  let json = Yojson.Safe.from_file filename in
  { version = json_int (json_field "version" json);
    data_points =
      json_list (json_field "data_points" json)
      |> StdList.map parse_data_point
  }

(* --- Heap dump parsing (JSONL) --- *)

let parse_heap_words_obj j =
  json_assoc j
  |> StdList.map (fun (addr_s, val_j) ->
    { addr = parse_hex_int64 addr_s;
      value = parse_hex_int64 (json_string val_j)
    })

let parse_heap_dump_line (line : string) : heap_dump =
  let j = Yojson.Safe.from_string line in
  { function_name = json_string (json_field "function" j);
    target_addr = parse_hex_int64 (json_string (json_field "addr" j));
    words = parse_heap_words_obj (json_field "words" j)
  }

let parse_heap_jsonl (filename : string) : heap_dump list =
  let ic = open_in filename in
  let rec read_lines acc =
    match input_line ic with
    | line ->
      let line = String.trim line in
      if String.length line = 0 then
        read_lines acc
      else
        read_lines (parse_heap_dump_line line :: acc)
    | exception End_of_file ->
      StdList.rev acc
  in
  Fun.protect ~finally:(fun () -> close_in ic)
    (fun () -> read_lines [])

(* --- Grouping --- *)

let group_by_function (dps : data_point list) : (string * data_point list) list =
  let tbl : (string, data_point list) Hashtbl.t = Hashtbl.create 16 in
  StdList.iter
    (fun (dp : data_point) ->
       let existing : data_point list =
         match Hashtbl.find_opt tbl dp.function_name with
         | Some l -> l
         | None -> []
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
         if offset >= e.size then set
         else
           let a = Int64.add e.addr (Int64.of_int offset) in
           add_range (Int64Set.add a set) (offset + 1)
       in
       add_range acc 0)
    Int64Set.empty
    entries

(* Build a lookup function from heap dumps: addr -> value option *)
let heap_lookup (dumps : heap_dump list) : int64 -> int64 option =
  let tbl = Hashtbl.create 256 in
  StdList.iter
    (fun dump ->
       StdList.iter
         (fun w -> Hashtbl.replace tbl w.addr w.value)
         dump.words)
    dumps;
  fun addr -> Hashtbl.find_opt tbl addr

(** Parsing and representation of bi-abductive execution data.
    Reads the .abd.json summary and .heap.jsonl heap dump files. *)

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
  { function_name : string;
    pre_vars : var_binding list;
    body_missing : missing_entry list; (** body auto-grants = precondition needs *)
    post_remaining : missing_entry list (** leak check remainder = postcondition *)
  }

type execution_data = { data_points : data_point list }

type heap_word =
  { addr : int64;
    value : int64
  }

type heap_dump = { words : heap_word list }

(** Parse a summary JSON file (.abd.json). *)
val parse_summary_json : string -> execution_data

(** Parse a heap dump JSONL file, split by phase.
    Returns (pre_dumps, post_dumps) where pre = H_entry, post = H_exit. *)
val parse_heap_jsonl : string -> heap_dump list * heap_dump list

(** Group data points by function name. *)
val group_by_function : data_point list -> (string * data_point list) list

module Int64Set : Set.S with type elt = int64

(** Collect all addresses from a missing entry list into a set. *)
val missing_addr_set : missing_entry list -> Int64Set.t

(** Build a lookup from heap dumps: given an address, return the 8-byte value if known. *)
val heap_lookup : heap_dump list -> int64 -> int64 option

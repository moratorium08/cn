(** Parsing and representation of bi-abductive execution data.
    Reads the .abd.json summary and .heap.jsonl heap dump files. *)

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
    body_missing : missing_entry list;    (** body auto-grants = precondition needs *)
    post_remaining : missing_entry list   (** leak check remainder = postcondition *)
  }

type execution_data =
  { version : int;
    data_points : data_point list
  }

type heap_word =
  { addr : int64;
    value : int64
  }

(** Phase tag for heap snapshots.
    - [Pre]  = H_entry: taken before body executes (at cn_abd_mark_post)
    - [Post] = H_exit:  taken after body executes (at cn_abd_record_post_remaining) *)
type dump_phase = Pre | Post

type heap_dump =
  { function_name : string;
    phase : dump_phase;
    target_addr : int64;
    words : heap_word list
  }

(** Parse a summary JSON file (.abd.json). *)
val parse_summary_json : string -> execution_data

(** Parse a heap dump JSONL file (.heap.jsonl). *)
val parse_heap_jsonl : string -> heap_dump list

(** Group data points by function name. *)
val group_by_function : data_point list -> (string * data_point list) list

module Int64Set : Set.S with type elt = int64

(** Collect all addresses from a missing entry list into a set. *)
val missing_addr_set : missing_entry list -> Int64Set.t

(** Build a lookup from heap dumps: given an address, return the 8-byte value if known. *)
val heap_lookup : heap_dump list -> (int64 -> int64 option)

(** Build a lookup from heap dumps filtered to the given phases. *)
val heap_lookup_for_phases : dump_phase list -> heap_dump list -> (int64 -> int64 option)

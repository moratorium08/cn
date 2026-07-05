(** Parsing and representation of bi-abductive execution data.
    Reads the cn_abd_summary.json summary and cn_abd_heap.jsonl heap dump
    files.  Both are keyed per activation ([dp_idx], call order). *)

module IntMap : Map.S with type key = int

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

(** Per-activation heap snapshots: dp index -> dumps for that activation. *)
type heap_dumps_by_dp = heap_dump list IntMap.t

(** Parse a summary JSON file (cn_abd_summary.json). *)
val parse_summary_json : string -> execution_data

(** Parse a heap dump JSONL file.  Returns (pre, post) snapshots keyed by
    activation: pre approximates H_entry, post is H_exit. *)
val parse_heap_jsonl : string -> heap_dumps_by_dp * heap_dumps_by_dp

(** Group data points by function name. *)
val group_by_function : data_point list -> (string * data_point list) list

module Int64Set : Set.S with type elt = int64

(** Collect all addresses from a missing entry list into a set. *)
val missing_addr_set : missing_entry list -> Int64Set.t

(** Flatten one activation's heap dumps into a deduplicated [(addr, value)]
    list, for feeding to the bi-abductive footprint harness. *)
val heap_words_for_dp : heap_dumps_by_dp -> int -> (int64 * int64) list

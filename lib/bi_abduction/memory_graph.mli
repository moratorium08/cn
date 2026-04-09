(** Memory graph for discovering structural patterns in heap data. *)

module Int64Set = Data_point.Int64Set
module Int64Map : Map.S with type key = int64

type edge =
  | Offset of int
  | Deref

type node_info =
  { is_anchor : bool;
    is_missing : bool
  }

type t

val empty : t

(** Access the per-node info map (for debug/stats). *)
val info : t -> node_info Int64Map.t

(** Build a memory graph from function arguments, a missing address set, and heap data.
    [pre_vars]     — function argument bindings used as BFS anchors.
    [missing_set]  — addresses considered missing for this graph
                     (body_missing for pre-graph; post_remaining for post-graph).
    [heap_lookup]  — phase-appropriate heap snapshot.
    [struct_layouts] maps struct tags to (field_id, offset, size) triples. *)
val build
  :  pre_vars:Data_point.var_binding list ->
  missing_set:Data_point.Int64Set.t ->
  heap_lookup:(int64 -> int64 option) ->
  struct_layouts:(Id.t * int * int) list Sym.Map.t ->
  t

(** All addresses reachable from a starting address. *)
val reachable_from : t -> int64 -> Int64Set.t

(** Does any path from the start address reach a missing address? *)
val connects_to_missing : t -> int64 -> bool

(** Set of anchor addresses (function arguments). *)
val anchors : t -> Int64Set.t

(** Set of missing addresses. *)
val missing : t -> Int64Set.t

(** All byte addresses within structs reachable from a start address.
    Expands struct bases along pointer chains to include full struct byte ranges. *)
val reachable_struct_bytes
  :  t -> int64 ->
  struct_layouts:(Id.t * int * int) list Sym.Map.t ->
  Int64Set.t

(** Memory graph: nodes = addresses, edges = offset or pointer-dereference.
    Used to discover structural patterns (linked lists, trees) in heap data
    that connect function arguments to missing ownership addresses. *)

module StdList = Stdlib.List
module Int64Set = Data_point.Int64Set
module Int64Map = Map.Make (Int64)

type edge =
  | Offset of int       (** Move by a fixed byte offset (struct field) *)
  | Deref               (** Follow a pointer dereference *)

type node_info =
  { is_anchor : bool;   (** Is this a function argument / known pointer? *)
    is_missing : bool    (** Is this address in the missing set? *)
  }

type t =
  { adj : (int64 * edge) list Int64Map.t;  (** Adjacency list *)
    info : node_info Int64Map.t            (** Per-node metadata *)
  }

let empty =
  { adj = Int64Map.empty;
    info = Int64Map.empty
  }

let add_node addr node_info g =
  let info = Int64Map.add addr node_info g.info in
  let adj =
    if Int64Map.mem addr g.adj then g.adj
    else Int64Map.add addr [] g.adj
  in
  { adj; info }

let add_edge src dst edge g =
  let existing =
    match Int64Map.find_opt src g.adj with
    | Some l -> l
    | None -> []
  in
  { g with adj = Int64Map.add src ((dst, edge) :: existing) g.adj }

(** Build a memory graph from a data point and heap data.

    Uses BFS expansion starting from anchor addresses (function arguments).
    For each address, overlays every struct layout to find fields, then
    follows pointer-sized fields via heap_lookup to discover new nodes.
    This allows the graph to trace through linked structures (lists, trees)
    even when intermediate nodes are not in the missing set.

    [struct_layouts] maps struct tag symbols to lists of (field_id, field_offset, field_size). *)
let build
      ~(dp : Data_point.data_point)
      ~(heap_lookup : int64 -> int64 option)
      ~(struct_layouts : (Id.t * int * int) list Sym.Map.t)
  =
  let missing_set =
    Int64Set.union
      (Data_point.missing_addr_set dp.pre_missing)
      (Data_point.missing_addr_set dp.post_missing)
  in
  let anchor_addrs =
    StdList.fold_left
      (fun acc (v : Data_point.var_binding) ->
         Int64Set.add v.value acc)
      Int64Set.empty
      (dp.pre_vars @ dp.post_vars)
  in
  let g = ref empty in
  (* Add all missing addresses as nodes *)
  Int64Set.iter
    (fun addr ->
       g := add_node addr
         { is_anchor = Int64Set.mem addr anchor_addrs;
           is_missing = true } !g)
    missing_set;
  (* BFS from anchors, following struct field pointers *)
  let visited = ref Int64Set.empty in
  let worklist = Queue.create () in
  let max_nodes = 10000 in
  (* Seed with anchor addresses *)
  Int64Set.iter
    (fun addr ->
       g := add_node addr
         { is_anchor = true;
           is_missing = Int64Set.mem addr missing_set } !g;
       Queue.add addr worklist)
    anchor_addrs;
  while not (Queue.is_empty worklist)
        && Int64Map.cardinal !g.info < max_nodes do
    let base = Queue.pop worklist in
    if not (Int64Set.mem base !visited) then begin
      visited := Int64Set.add base !visited;
      (* Try overlaying each struct layout at this base address *)
      Sym.Map.iter
        (fun _tag fields ->
           StdList.iter
             (fun (_field_id, offset, size) ->
                let field_addr = Int64.add base (Int64.of_int offset) in
                (* Add field node if not already present *)
                if not (Int64Map.mem field_addr !g.info) then
                  g := add_node field_addr
                    { is_anchor = false;
                      is_missing = Int64Set.mem field_addr missing_set } !g;
                (* Add Offset edge from base to field (skip self-edges) *)
                if offset > 0 then
                  g := add_edge base field_addr (Offset offset) !g;
                (* If field is pointer-sized (8 bytes), try dereference *)
                if size = 8 then begin
                  match heap_lookup field_addr with
                  | Some target when Int64.compare target 0L <> 0 ->
                    (* Non-NULL pointer target: add node and Deref edge *)
                    if not (Int64Map.mem target !g.info) then
                      g := add_node target
                        { is_anchor = false;
                          is_missing = Int64Set.mem target missing_set } !g;
                    g := add_edge field_addr target Deref !g;
                    (* Enqueue target for further exploration *)
                    if not (Int64Set.mem target !visited) then
                      Queue.add target worklist
                  | _ -> ()
                end)
             fields)
        struct_layouts
    end
  done;
  !g

(** All addresses reachable from [start] via any edges. *)
let reachable_from (g : t) (start : int64) : Int64Set.t =
  let visited = ref Int64Set.empty in
  let rec walk addr =
    if Int64Set.mem addr !visited then ()
    else begin
      visited := Int64Set.add addr !visited;
      match Int64Map.find_opt addr g.adj with
      | None -> ()
      | Some edges ->
        StdList.iter (fun (dst, _) -> walk dst) edges
    end
  in
  walk start;
  !visited

(** Check if any address reachable from [start] is in the missing set. *)
let connects_to_missing (g : t) (start : int64) : bool =
  let reachable = reachable_from g start in
  Int64Set.exists
    (fun addr ->
       match Int64Map.find_opt addr g.info with
       | Some info -> info.is_missing
       | None -> false)
    reachable

(** Return the set of anchor addresses. *)
let anchors (g : t) : Int64Set.t =
  Int64Map.fold
    (fun addr info acc ->
       if info.is_anchor then Int64Set.add addr acc else acc)
    g.info
    Int64Set.empty

(** Return the set of missing addresses. *)
let missing (g : t) : Int64Set.t =
  Int64Map.fold
    (fun addr info acc ->
       if info.is_missing then Int64Set.add addr acc else acc)
    g.info
    Int64Set.empty

(** Compute the total size of each struct layout (max of offset + size across fields). *)
let struct_total_sizes
      (struct_layouts : (Id.t * int * int) list Sym.Map.t)
  : int list
  =
  Sym.Map.fold
    (fun _tag fields acc ->
       let total =
         StdList.fold_left
           (fun mx (_fid, off, sz) -> max mx (off + sz))
           0 fields
       in
       total :: acc)
    struct_layouts []

(** All byte addresses within structs reachable from [start].
    For each struct base address reachable via pointer chains,
    expands to include all bytes in [base, base + struct_size).
    This gives the full ownership footprint of a recursive predicate. *)
let reachable_struct_bytes
      (g : t)
      (start : int64)
      ~(struct_layouts : (Id.t * int * int) list Sym.Map.t)
  : Int64Set.t
  =
  let sizes = struct_total_sizes struct_layouts in
  let reachable = reachable_from g start in
  (* Identify struct base addresses: those with outgoing Offset or Deref edges,
     or that are targets of Deref edges. We approximate by checking if any
     struct field offset from the address also appears in the graph. *)
  Int64Set.fold
    (fun addr acc ->
       (* Check if this address looks like a struct base by checking
          whether it has outgoing edges (offset or was explored) *)
       let has_outgoing =
         match Int64Map.find_opt addr g.adj with
         | Some (_ :: _) -> true
         | _ -> false
       in
       (* Also consider it a struct base if it's a Deref target or anchor *)
       let is_base =
         has_outgoing ||
         (match Int64Map.find_opt addr g.info with
          | Some info -> info.is_anchor
          | None -> false)
       in
       if is_base then
         (* Expand: add all bytes [addr, addr + struct_size) for each struct *)
         StdList.fold_left
           (fun acc2 total_size ->
              let rec add_bytes s offset =
                if offset >= total_size then s
                else
                  add_bytes
                    (Int64Set.add (Int64.add addr (Int64.of_int offset)) s)
                    (offset + 1)
              in
              add_bytes acc2 0)
           acc sizes
       else
         Int64Set.add addr acc)
    reachable
    Int64Set.empty

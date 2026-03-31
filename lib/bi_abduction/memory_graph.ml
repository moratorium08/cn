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

    - Anchors: addresses that appear in variable valuations (function args).
    - Missing: addresses in the missing set.
    - Edges: For each anchor/reachable address, if the 8-byte value at that address
      looks like a pointer (appears as another node or is close to one), add a Deref edge.
      For struct fields, add Offset edges between the base and field addresses.

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
  (* Collect all "interesting" addresses: anchors + missing *)
  let anchor_addrs =
    StdList.fold_left
      (fun acc (v : Data_point.var_binding) ->
         Int64Set.add v.value acc)
      Int64Set.empty
      (dp.pre_vars @ dp.post_vars)
  in
  let all_addrs = Int64Set.union anchor_addrs missing_set in
  (* Initialize graph with known nodes *)
  let g = ref empty in
  Int64Set.iter
    (fun addr ->
       let info =
         { is_anchor = Int64Set.mem addr anchor_addrs;
           is_missing = Int64Set.mem addr missing_set
         }
       in
       g := add_node addr info !g)
    all_addrs;
  (* Add struct offset edges: for each anchor, try each struct layout *)
  Int64Set.iter
    (fun base_addr ->
       Sym.Map.iter
         (fun _tag fields ->
            StdList.iter
              (fun (_field_id, offset, _size) ->
                 let field_addr = Int64.add base_addr (Int64.of_int offset) in
                 if Int64Set.mem field_addr all_addrs || offset = 0 then begin
                   (* Add the field node if not present *)
                   if not (Int64Map.mem field_addr !g.info) then
                     g := add_node field_addr
                       { is_anchor = false;
                         is_missing = Int64Set.mem field_addr missing_set
                       } !g;
                   if offset > 0 then
                     g := add_edge base_addr field_addr (Offset offset) !g
                 end)
              fields)
         struct_layouts)
    anchor_addrs;
  (* Add dereference edges: for each address in the graph, look up its value
     in the heap dump. If the value matches another address in the graph, add Deref. *)
  let current_addrs =
    Int64Map.fold (fun k _ acc -> Int64Set.add k acc) !g.info Int64Set.empty
  in
  Int64Set.iter
    (fun addr ->
       (* Read the 8-byte value at this address *)
       match heap_lookup addr with
       | Some pointed_to when Int64Set.mem pointed_to current_addrs ->
         g := add_edge addr pointed_to Deref !g
       | _ -> ())
    current_addrs;
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

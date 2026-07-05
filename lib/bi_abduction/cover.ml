(** Disjoint set cover across all data points (paper: Definition
    "Data-relative inference", condition (†)).

    A candidate carries one footprint per data point.  The cover must, for
    every data point j, cover the whole lower bound A_j with footprints that
    are pairwise disjoint on j.  Selection is greedy by total newly-covered
    bytes across data points; ties prefer the candidate with the least
    over-approximation Σ_j |F_j \ A_j| (the concrete analogue of the least
    anti-frame). *)

module StdList = Stdlib.List
module Int64Set = Data_point.Int64Set
module IntMap = Data_point.IntMap

type footprint = Int64Set.t

type candidate =
  { qualifier : Qualifier.t;
    footprints : footprint IntMap.t (** dp_idx -> footprint on that data point *)
  }

type cover_result =
  { selected : Qualifier.t list;
    uncovered : Int64Set.t IntMap.t (** per-dp bytes of A left uncovered *)
  }

let all_empty (m : Int64Set.t IntMap.t) : bool =
  IntMap.for_all (fun _ s -> Int64Set.is_empty s) m


(** [c] is usable iff its footprint is disjoint, on every data point, from
    what the already-selected candidates cover there. *)
let is_disjoint (covered : Int64Set.t IntMap.t) (c : candidate) : bool =
  IntMap.for_all
    (fun dp_idx fp ->
       match IntMap.find_opt dp_idx covered with
       | None -> true
       | Some cov -> Int64Set.is_empty (Int64Set.inter cov fp))
    c.footprints


let score (remaining : Int64Set.t IntMap.t) (c : candidate) : int =
  IntMap.fold
    (fun dp_idx rem acc ->
       match IntMap.find_opt dp_idx c.footprints with
       | None -> acc
       | Some fp -> acc + Int64Set.cardinal (Int64Set.inter rem fp))
    remaining
    0


(** Over-approximation of a candidate w.r.t. the lower bounds:
    Σ_j |F_j \ A_j|.  Used as a tie-break so the cover stays close to the
    least solution. *)
let over_approx (must_cover : Int64Set.t IntMap.t) (c : candidate) : int =
  IntMap.fold
    (fun dp_idx fp acc ->
       let a =
         match IntMap.find_opt dp_idx must_cover with
         | None -> Int64Set.empty
         | Some a -> a
       in
       acc + Int64Set.cardinal (Int64Set.diff fp a))
    c.footprints
    0


let greedy_cover ~(must_cover : Int64Set.t IntMap.t) ~(candidates : candidate list)
  : cover_result
  =
  let scored_over = StdList.map (fun c -> (c, over_approx must_cover c)) candidates in
  let rec loop selected covered remaining pool =
    if all_empty remaining then
      { selected = StdList.rev selected; uncovered = remaining }
    else (
      let usable =
        StdList.filter_map
          (fun (c, over) ->
             if is_disjoint covered c then (
               let s = score remaining c in
               if s > 0 then Some (c, s, over) else None)
             else
               None)
          pool
      in
      match usable with
      | [] -> { selected = StdList.rev selected; uncovered = remaining }
      | first :: rest ->
        let best, _, _ =
          StdList.fold_left
            (fun (bc, bs, bo) (c, s, o) ->
               if s > bs || (s = bs && o < bo) then (c, s, o) else (bc, bs, bo))
            first
            rest
        in
        let covered' =
          IntMap.fold
            (fun dp_idx fp acc ->
               IntMap.update
                 dp_idx
                 (function Some cov -> Some (Int64Set.union cov fp) | None -> Some fp)
                 acc)
            best.footprints
            covered
        in
        let remaining' =
          IntMap.mapi
            (fun dp_idx rem ->
               match IntMap.find_opt dp_idx best.footprints with
               | None -> rem
               | Some fp -> Int64Set.diff rem fp)
            remaining
        in
        let pool' =
          StdList.filter
            (fun (c, _) -> not (Qualifier.equal c.qualifier best.qualifier))
            scored_over
        in
        loop (best.qualifier :: selected) covered' remaining' pool')
  in
  loop [] IntMap.empty must_cover scored_over


let cover = greedy_cover

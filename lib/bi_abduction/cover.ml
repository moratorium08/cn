(** Disjoint set cover algorithm for selecting qualifiers.

    Given a set of candidate qualifiers with precomputed footprints
    (sets of addresses each qualifier covers), find a minimal subset
    that covers all missing addresses while maintaining disjointness. *)

module StdList = Stdlib.List
module Int64Set = Data_point.Int64Set

type footprint = Int64Set.t

type candidate =
  { qualifier : Qualifier.t;
    footprint : footprint
  }

type cover_result =
  { selected : Qualifier.t list;
    uncovered : Int64Set.t
  }

(** Check if a candidate's footprint is disjoint with already-covered addresses. *)
let is_disjoint (covered : Int64Set.t) (fp : footprint) : bool =
  Int64Set.is_empty (Int64Set.inter covered fp)

(** Greedy disjoint set cover.
    Repeatedly select the candidate covering the most uncovered addresses,
    maintaining disjointness with previously selected candidates. *)
let greedy_cover
      ~(must_cover : Int64Set.t)
      ~(candidates : candidate list)
  : cover_result
  =
  let rec loop selected covered remaining_must remaining_candidates =
    if Int64Set.is_empty remaining_must then
      { selected = StdList.rev selected; uncovered = Int64Set.empty }
    else
      (* Score each remaining candidate: number of must-cover addresses it covers *)
      let scored =
        StdList.filter_map
          (fun c ->
             if is_disjoint covered c.footprint then
               let covers = Int64Set.inter remaining_must c.footprint in
               let score = Int64Set.cardinal covers in
               if score > 0 then Some (c, score)
               else None
             else None)
          remaining_candidates
      in
      match scored with
      | [] ->
        (* No more candidates can cover remaining addresses *)
        { selected = StdList.rev selected; uncovered = remaining_must }
      | _ ->
        (* Pick the highest-scoring candidate *)
        let best, _best_score =
          StdList.fold_left
            (fun (best, bs) (c, s) -> if s > bs then (c, s) else (best, bs))
            (StdList.hd scored)
            (StdList.tl scored)
        in
        let new_covered = Int64Set.union covered best.footprint in
        let new_remaining = Int64Set.diff remaining_must best.footprint in
        let new_candidates =
          StdList.filter (fun c -> not (Qualifier.equal c.qualifier best.qualifier))
            remaining_candidates
        in
        loop (best.qualifier :: selected) new_covered new_remaining new_candidates
  in
  loop [] Int64Set.empty must_cover candidates

(** Exact cover for small candidate sets (enumerate all subsets).
    Falls back to greedy if too many candidates. *)
let exact_cover
      ~(must_cover : Int64Set.t)
      ~(candidates : candidate list)
      ~(max_subset_size : int)
  : cover_result
  =
  let n = StdList.length candidates in
  if n > max_subset_size then
    greedy_cover ~must_cover ~candidates
  else begin
    (* Try all subsets of size 1, then 2, etc. *)
    let candidates_arr = Array.of_list candidates in
    let best = ref { selected = []; uncovered = must_cover } in
    let rec try_subsets chosen covered idx remaining =
      if Int64Set.is_empty remaining then begin
        (* Found a complete cover *)
        let selected = StdList.rev_map (fun i -> candidates_arr.(i).qualifier) chosen in
        if StdList.length selected < StdList.length !best.selected
           || not (Int64Set.is_empty !best.uncovered) then
          best := { selected; uncovered = Int64Set.empty }
      end else if idx >= n then
        ()
      else begin
        (* Try including candidate idx *)
        let c = candidates_arr.(idx) in
        if is_disjoint covered c.footprint then begin
          let new_covered = Int64Set.union covered c.footprint in
          let new_remaining = Int64Set.diff remaining c.footprint in
          try_subsets (idx :: chosen) new_covered (idx + 1) new_remaining
        end;
        (* Try not including candidate idx *)
        try_subsets chosen covered (idx + 1) remaining
      end
    in
    try_subsets [] Int64Set.empty 0 must_cover;
    !best
  end

(** Main cover function: use exact for small sets, greedy for large. *)
let cover
      ~(must_cover : Int64Set.t)
      ~(candidates : candidate list)
  : cover_result
  =
  let n = StdList.length candidates in
  if n <= 20 then
    exact_cover ~must_cover ~candidates ~max_subset_size:20
  else
    greedy_cover ~must_cover ~candidates

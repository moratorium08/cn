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
let greedy_cover ~(must_cover : Int64Set.t) ~(candidates : candidate list) : cover_result =
  let rec loop selected covered remaining_must remaining_candidates =
    if Int64Set.is_empty remaining_must then
      { selected = StdList.rev selected; uncovered = Int64Set.empty }
    else (
      (* Score each remaining candidate: number of must-cover addresses it covers *)
      let scored =
        StdList.filter_map
          (fun c ->
             if is_disjoint covered c.footprint then (
               let covers = Int64Set.inter remaining_must c.footprint in
               let score = Int64Set.cardinal covers in
               if score > 0 then
                 Some (c, score)
               else
                 None)
             else
               None)
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
          StdList.filter_map
            (fun (c, _) ->
               if Qualifier.equal c.qualifier best.qualifier then None else Some c)
            scored
        in
        loop (best.qualifier :: selected) new_covered new_remaining new_candidates)
  in
  loop [] Int64Set.empty must_cover candidates


let cover = greedy_cover

(** Disjoint set cover across all data points (paper: Definition
    "Data-relative inference", condition (†)), with chain-prefix sharing
    (IDEA.md 4.4).

    A candidate is a qualifier decomposed into *canonical steps*, each with
    one footprint per data point.  Two candidates may share a step (same
    [key], e.g. the [take W = RW<struct S>(b)] prefix of two chains rooted
    at the same argument); a shared step's footprint is counted once, which
    is what makes [take W = ...; take _ = P(W.xs); take _ = P(W.ys)]
    coverable without a spurious disjointness conflict.  Distinct selected
    steps must have pairwise-disjoint footprints on every data point.

    Selection is greedy by total newly-covered A bytes across data points;
    ties prefer the candidate with the least over-approximation
    Σ_j |F_j \ A_j| over its new steps (the concrete analogue of the least
    anti-frame). *)

module StdList = Stdlib.List
module StringSet = Set.Make (Stdlib.String)
module Int64Set = Data_point.Int64Set
module IntMap = Data_point.IntMap

type step_unit =
  { key : string; (** canonical identity: same key ⇒ same step, shareable *)
    footprints : Int64Set.t IntMap.t (** dp_idx -> footprint on that data point *)
  }

type candidate =
  { qualifier : Qualifier.t;
    steps : step_unit list
  }

type cover_result =
  { selected : Qualifier.t list;
    uncovered : Int64Set.t IntMap.t (** per-dp bytes of A left uncovered *)
  }

let all_empty (m : Int64Set.t IntMap.t) : bool =
  IntMap.for_all (fun _ s -> Int64Set.is_empty s) m


let new_steps (selected_keys : StringSet.t) (c : candidate) : step_unit list =
  StdList.filter (fun s -> not (StringSet.mem s.key selected_keys)) c.steps


let step_disjoint (covered : Int64Set.t IntMap.t) (s : step_unit) : bool =
  IntMap.for_all
    (fun dp_idx fp ->
       match IntMap.find_opt dp_idx covered with
       | None -> true
       | Some cov -> Int64Set.is_empty (Int64Set.inter cov fp))
    s.footprints


let add_step (covered : Int64Set.t IntMap.t) (s : step_unit) : Int64Set.t IntMap.t =
  IntMap.fold
    (fun dp_idx fp acc ->
       IntMap.update
         dp_idx
         (function Some cov -> Some (Int64Set.union cov fp) | None -> Some fp)
         acc)
    s.footprints
    covered


let steps_score (remaining : Int64Set.t IntMap.t) (steps : step_unit list) : int =
  StdList.fold_left
    (fun acc (s : step_unit) ->
       IntMap.fold
         (fun dp_idx fp inner ->
            match IntMap.find_opt dp_idx remaining with
            | None -> inner
            | Some rem -> inner + Int64Set.cardinal (Int64Set.inter rem fp))
         s.footprints
         acc)
    0
    steps


(** Over-approximation w.r.t. the lower bounds: Σ_j |F_j \ A_j| over the
    given steps. *)
let steps_over (must_cover : Int64Set.t IntMap.t) (steps : step_unit list) : int =
  StdList.fold_left
    (fun acc (s : step_unit) ->
       IntMap.fold
         (fun dp_idx fp inner ->
            let a =
              match IntMap.find_opt dp_idx must_cover with
              | None -> Int64Set.empty
              | Some a -> a
            in
            inner + Int64Set.cardinal (Int64Set.diff fp a))
         s.footprints
         acc)
    0
    steps


let greedy_cover ~(must_cover : Int64Set.t IntMap.t) ~(candidates : candidate list)
  : cover_result
  =
  let rec loop selected selected_keys covered remaining pool =
    if all_empty remaining then
      { selected = StdList.rev selected; uncovered = remaining }
    else (
      let usable =
        StdList.filter_map
          (fun c ->
             let steps = new_steps selected_keys c in
             if StdList.for_all (step_disjoint covered) steps then (
               let s = steps_score remaining steps in
               if s > 0 then Some (c, steps, s, steps_over must_cover steps) else None)
             else
               None)
          pool
      in
      match usable with
      | [] -> { selected = StdList.rev selected; uncovered = remaining }
      | first :: rest ->
        let best, best_steps, _, _ =
          StdList.fold_left
            (fun (bc, bst, bs, bo) (c, st, s, o) ->
               if s > bs || (s = bs && o < bo) then (c, st, s, o) else (bc, bst, bs, bo))
            first
            rest
        in
        let covered' = StdList.fold_left add_step covered best_steps in
        let selected_keys' =
          StdList.fold_left
            (fun acc (s : step_unit) -> StringSet.add s.key acc)
            selected_keys
            best_steps
        in
        let remaining' =
          StdList.fold_left
            (fun rem (s : step_unit) ->
               IntMap.mapi
                 (fun dp_idx r ->
                    match IntMap.find_opt dp_idx s.footprints with
                    | None -> r
                    | Some fp -> Int64Set.diff r fp)
                 rem)
            remaining
            best_steps
        in
        let pool' =
          StdList.filter (fun c -> not (Qualifier.equal c.qualifier best.qualifier)) pool
        in
        loop (best.qualifier :: selected) selected_keys' covered' remaining' pool')
  in
  loop [] StringSet.empty IntMap.empty must_cover candidates


let cover = greedy_cover

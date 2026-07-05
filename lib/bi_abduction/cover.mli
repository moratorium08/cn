(** Disjoint set cover across all data points (paper: condition (†)),
    with chain-prefix sharing (IDEA.md 4.4). *)

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

val greedy_cover
  :  must_cover:Int64Set.t IntMap.t ->
  candidates:candidate list ->
  cover_result

val cover : must_cover:Int64Set.t IntMap.t -> candidates:candidate list -> cover_result

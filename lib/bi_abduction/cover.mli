(** Disjoint set cover across all data points (paper: condition (†)). *)

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

val greedy_cover
  :  must_cover:Int64Set.t IntMap.t ->
  candidates:candidate list ->
  cover_result

val cover : must_cover:Int64Set.t IntMap.t -> candidates:candidate list -> cover_result

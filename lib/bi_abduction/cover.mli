(** Disjoint set cover algorithm for selecting qualifiers. *)

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

val greedy_cover : must_cover:Int64Set.t -> candidates:candidate list -> cover_result

val exact_cover
  :  must_cover:Int64Set.t ->
  candidates:candidate list ->
  max_subset_size:int ->
  cover_result

val cover : must_cover:Int64Set.t -> candidates:candidate list -> cover_result

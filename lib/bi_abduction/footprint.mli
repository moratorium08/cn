(** Footprint computation for qualifiers. *)

module Int64Set = Data_point.Int64Set

val owned_footprint : ct:Sctypes.t -> base_addr:int64 -> Int64Set.t

val compute : Qualifier.t -> Data_point.data_point -> Int64Set.t option

val compute_batch
  :  Qualifier.t list ->
  Data_point.data_point ->
  (Qualifier.t * Int64Set.t option) list

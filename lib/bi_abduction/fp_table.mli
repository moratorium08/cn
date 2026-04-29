(** Footprint lookup table indexed by qualifier id.

    Populated by [Fp_runner.run] from the harness's JSON output and
    consulted by the inference pipeline for predicate qualifiers
    (Owned qualifiers are computed analytically and never appear here). *)

module Int64Set = Data_point.Int64Set

type t

val empty : t

(** [find t q_idx] returns:
    - [None] if no entry exists (qualifier was never sent to the harness)
    - [Some None] if the harness ran and reported a miss/failure
    - [Some (Some fp)] if the harness produced a concrete footprint *)
val find : t -> int -> Int64Set.t option option

val of_results : (int * Int64Set.t option) list -> t

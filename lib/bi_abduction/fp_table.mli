(** Footprint lookup table indexed by (qualifier id, data-point id).

    Populated by [Fp_runner.run] from the harness's JSON output and
    consulted by the inference pipeline for predicate qualifiers
    (Owned qualifiers are computed analytically and never appear here). *)

module Int64Set = Data_point.Int64Set

type t

val empty : t

(** [find t (q_idx, dp_idx)] returns:
    - [None] if no entry exists (the qualifier was not run against
      this dp — typically because some free symbol of the qualifier
      was missing from the dp's pre_vars and the codegen filtered it)
    - [Some None] if the harness ran and reported a miss/failure
    - [Some (Some fp)] if the harness produced a concrete footprint *)
val find : t -> int * int -> Int64Set.t option option

val of_results : (int * int * Int64Set.t option) list -> t

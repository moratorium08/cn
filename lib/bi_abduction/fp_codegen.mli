(** Emit a self-contained C harness that runs each predicate qualifier in
    PRE mode against a recorded heap and reports the resulting ghost-state
    delta as the qualifier's footprint.

    The generated C is symbolic: per-qualifier functions reference free
    symbols via [fp_var_value(dp, "name")] looked up at runtime against a
    static [ALL_DPS] table, instead of baking a particular data point's
    resolved values in as literals.  This keeps the C source readable as
    "evaluate qualifier Q against scope dp" and lets the same harness
    sweep over an arbitrary list of data points without re-codegen.

    The caller is responsible for filtering qualifiers down to predicate
    qualifiers (Owned ones are computed analytically by
    [Footprint.owned_footprint]).  Per-(qualifier, dp) validity is decided
    at codegen time inside [emit] by intersecting each qualifier's free
    symbols with each dp's [pre_vars] names. *)

module CF = Cerb_frontend

(** A data point as the harness consumes it: an arbitrary integer index
    (used as the [dp] field in the JSON output), the [data_point] record
    (for variable bindings), and a flattened heap snapshot. *)
type dp_entry =
  { dp_idx : int;
    dp : Data_point.data_point;
    heap_words : (int64 * int64) list
  }

type input =
  { filename : string;
    cabs_tunit : CF.Cabs.translation_unit;
    ail_prog : CF.GenTypes.genTypeCategory CF.AilSyntax.sigma;
    prog5 : unit Mucore.file;
    pred_defs : Definition.Predicate.t Sym.Map.t;
    data_points : dp_entry list;
        (** All data points the harness should sweep over.  Today the
            inference layer passes a singleton (the representative dp);
            the codegen is structured so this can grow to N entries
            without a code change. *)
    qualifiers : (int * Qualifier.t) list;
        (** [(q_idx, qualifier)] pairs.  Only [Request.P] with [PName _]
            should appear here. *)
    output_json_path : string
        (** File the harness should write JSON results to. *)
  }

(** Emit the full harness C source. *)
val emit : input -> string

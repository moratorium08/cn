(** Emit a self-contained C harness that runs each predicate qualifier in
    PRE mode against a reconstructed heap and reports the resulting
    ghost-state delta as the qualifier's footprint.

    The caller is responsible for:
    - filtering qualifiers down to predicate qualifiers (Owned ones are
      computed analytically by [Footprint.owned_footprint]);
    - pre-validating that every free symbol used in a qualifier resolves
      against the matching data point's [pre_vars], so the C side never
      has to handle missing-symbol cases. *)

module CF = Cerb_frontend

type input =
  { filename : string;
    cabs_tunit : CF.Cabs.translation_unit;
    ail_prog : CF.GenTypes.genTypeCategory CF.AilSyntax.sigma;
    prog5 : unit Mucore.file;
    pred_defs : Definition.Predicate.t Sym.Map.t;
    representative_dp : Data_point.data_point;
        (** The data point against which qualifiers are evaluated. *)
    heap_words : (int64 * int64) list;
        (** Flattened heap snapshot ([address] → 8-byte word value).
            Used to back the [cn_load_hook]. *)
    qualifiers : (int * Qualifier.t) list;
        (** [(q_idx, qualifier)] pairs.  Only [Request.P] with [PName _]
            should appear here. *)
    output_json_path : string
        (** File the harness should write JSON results to. *)
  }

(** Emit the full harness C source. *)
val emit : input -> string

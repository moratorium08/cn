/* Predicate arguments, definitions and return values are values: a boolean
 * there must be exported as bool, not as a Prop that cannot be passed on, and
 * boolean equality on datatypes needs a decidable equality, not Z.eqb. */
/*@
datatype opt_u32 {
  Opt_none {},
  Opt_some { u32 value }
}

predicate (boolean) Flagged(pointer p, boolean flag, datatype opt_u32 o) {
  take v = RW<unsigned int>(p);
  let both = flag && v == 0;
  let none = o == Opt_none {};
  return both && none;
}

lemma flagged_identity(pointer p)
  requires take before = Flagged(p, true, Opt_none {});
  ensures take after = Flagged(p, true, Opt_none {}); after == before;
@*/

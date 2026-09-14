/* Exportable, but false in general: W does not imply a zero output.
 * The Rocq regression checks an obstruction to proving this statement. */
/*@
lemma unsupported_w_output(pointer p)
  requires take before = each(integer i; 0 <= i && i < 1) { W<unsigned int>(array_shift<unsigned int>(p, i)) };
  ensures before[0] == 0;
@*/

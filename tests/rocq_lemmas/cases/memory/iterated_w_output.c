/* Must reject, not drop the output binding or invent readable W values. */
/*@
lemma unsupported_w_output(pointer p)
  requires take before = each(u64 i; i < 1u64) { W<unsigned int>(array_shift<unsigned int>(p, i)) };
  ensures before[0u64] == 0u32;
@*/

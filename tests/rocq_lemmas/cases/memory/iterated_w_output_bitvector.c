/* Even a finite-width W ghost map is arbitrary within its type range.
 * This false zero claim must remain unprovable. */
/*@
lemma unsupported_w_ghost(pointer p)
  requires take before = each (u64 i; i == 0u64) {
    W<unsigned int>(array_shift<unsigned int>(p, i))
  };
  ensures before[0u64] == 0u32;
@*/

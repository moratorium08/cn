/* A finite-width ghost map needs codomain range constraints. Until those
 * are exported, reject it rather than allow an arbitrary Z-valued witness. */
/*@
lemma unsupported_w_ghost(pointer p)
  requires take before = each (u64 i; i == 0u64) {
    W<unsigned int>(array_shift<unsigned int>(p, i))
  };
  ensures before[0u64] == 0u32;
@*/

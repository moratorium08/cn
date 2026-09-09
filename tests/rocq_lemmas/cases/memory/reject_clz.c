/* Must be rejected before the destination file is opened. */
/*@
lemma unsupported_clz()
  requires true;
  ensures bw_clz(1u64) == 63u64;
@*/

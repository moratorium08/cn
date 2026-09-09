/* These fixtures work with the bitvector frontend. Integer-mode fixtures
 * are separate so that neither frontend's index type is silently changed. */
[[cerb::byte]] typedef unsigned char byte;
/*@
predicate void SparseW(pointer p) {
  take cells = each(u64 i; i == 1u64 || i == 3u64) {
    W<unsigned int>(array_shift<unsigned long>(p, i))
  };
  return;
}

lemma each_w_identity(pointer p, u64 n)
  requires take before = each(u64 i; i < n) { W<byte>(array_shift<byte>(p, i)) };
  ensures take after = each(u64 i; i < n) { W<byte>(array_shift<byte>(p, i)) };

lemma each_w_sparse_identity(pointer p)
  requires take before = each(u64 i; i == 1u64 || i == 3u64) {
    W<unsigned int>(array_shift<unsigned long>(p, i))
  };
  ensures take after = SparseW(p);

lemma each_w_sparse_unpack(pointer p)
  requires take before = SparseW(p);
  ensures take after = each(u64 i; i == 1u64 || i == 3u64) {
    W<unsigned int>(array_shift<unsigned long>(p, i))
  };

lemma each_w_empty_has_id(pointer p)
  requires take before = each(u64 i; false) { W<byte>(array_shift<byte>(p, i)) };
  ensures !is_null(p);

lemma each_rw_map_identity(pointer p)
  requires take before = each(u64 i; i < 2u64) { RW<unsigned int>(array_shift<unsigned int>(p, i)) };
    before[0u64] == 42u32;
  ensures take after = each(u64 i; i < 2u64) { RW<unsigned int>(array_shift<unsigned int>(p, i)) };
    after[0u64] == 42u32;
@*/

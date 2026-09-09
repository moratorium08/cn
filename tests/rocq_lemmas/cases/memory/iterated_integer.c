[[cerb::byte]] typedef unsigned char byte;
/*@
predicate void NegativeSparseW(pointer p) {
  take cells = each(integer i; i + 2 == 0 || i == 3) {
    W<unsigned int>(array_shift<unsigned long>(p, i))
  };
  return;
}

lemma each_w_negative_sparse(pointer p)
  requires take before = each(integer i; i + 2 == 0 || i == 3) {
    W<unsigned int>(array_shift<unsigned long>(p, i))
  };
  ensures take after = NegativeSparseW(p);

lemma each_w_integer_identity(pointer p, integer n)
  requires take before = each(integer i; 0 <= i && i < n) {
    W<byte>(array_shift<byte>(p, i))
  };
  ensures take after = each(integer i; 0 <= i && i < n) {
    W<byte>(array_shift<byte>(p, i))
  };

lemma each_rw_signed_map(pointer p)
  requires take before = each(integer i; 0 <= i && i < 2) {
    RW<int>(array_shift<int>(p, i))
  };
    before[0] + 1 == 0;
  ensures take after = each(integer i; 0 <= i && i < 2) {
    RW<int>(array_shift<int>(p, i))
  };
    after[0] + 1 == 0;
@*/

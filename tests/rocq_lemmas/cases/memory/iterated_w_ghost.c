/* W's output is a logical map, with no claim about readable byte values.
 * Exercise requires, ensures and predicate-clause binders independently. */
/*@
predicate (map<integer, integer>) WMap(pointer p, integer n) {
  take values = each (integer i; 0 <= i && i < n) {
    W<unsigned int>(array_shift<unsigned int>(p, i))
  };
  return values;
}

lemma w_ghost_preserve(pointer p, integer n)
  requires take before = each (integer i; 0 <= i && i < n) {
    W<unsigned int>(array_shift<unsigned int>(p, i))
  };
  ensures take after = each (integer i; 0 <= i && i < n) {
    W<unsigned int>(array_shift<unsigned int>(p, i))
  };
  after == before;

lemma w_ghost_pack(pointer p, integer n)
  requires take before = each (integer i; 0 <= i && i < n) {
    W<unsigned int>(array_shift<unsigned int>(p, i))
  };
  ensures take after = WMap(p, n); after == before;

lemma w_ghost_unpack(pointer p, integer n)
  requires take before = WMap(p, n);
  ensures take after = each (integer i; 0 <= i && i < n) {
    W<unsigned int>(array_shift<unsigned int>(p, i))
  };
  after == before;

// No implicit nonnegative/range/zero constraint on an unsigned W output.
lemma w_ghost_negative(pointer p)
  requires take before = each (integer i; 0 <= i && i < 1) {
    W<unsigned int>(array_shift<unsigned int>(p, i))
  };
  before[0] == -1;
  ensures take after = each (integer i; 0 <= i && i < 1) {
    W<unsigned int>(array_shift<unsigned int>(p, i))
  };
  after[0] == -1;
@*/

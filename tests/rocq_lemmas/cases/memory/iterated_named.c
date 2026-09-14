/* Named resources keep their arguments, output maps, and recursive calls. */
/*@
predicate (integer) Cell(pointer p, integer value, boolean enabled) {
  take v = RW<int>(p);
  assert(enabled);
  assert(v == value);
  return v;
}
predicate (map<integer, integer>) Cells(pointer p, integer n) {
  take cells = each (integer i; 0 <= i && i < n) {
    Cell(array_shift<int>(p, i), i, true)
  };
  return cells;
}
predicate [rec] (void) Nest(pointer p, integer n) {
  if (n <= 0) { return; }
  else {
    take children = each (integer i; 0 <= i && i < n) {
      Nest(array_shift<int>(p, i), 0)
    };
    return;
  }
}
lemma named_pack(pointer p, integer n)
  requires take before = each (integer i; 0 <= i && i < n) {
    Cell(array_shift<int>(p, i), i, true)
  };
  ensures take after = Cells(p, n); before == after;
lemma named_unpack(pointer p, integer n)
  requires take before = Cells(p, n);
  ensures take after = each (integer i; 0 <= i && i < n) {
    Cell(array_shift<int>(p, i), i, true)
  }; before == after;
lemma named_empty()
  requires true;
  ensures take empty = each (integer i; 0 <= i && i < 0) {
    Cell(array_shift<int>(NULL, i), i, true)
  };
lemma named_singleton(pointer p)
  requires take before = Cell(array_shift<int>(p, 0), 17, true);
  ensures take after = each (integer i; 0 <= i && i < 1) {
    Cell(array_shift<int>(p, i), 17 + i, i == 0)
  }; after[0] == before;
lemma named_recursive_step(pointer p, integer n)
  requires n > 0;
    take children = each (integer i; 0 <= i && i < n) {
      Nest(array_shift<int>(p, i), 0)
    };
  ensures take nested = Nest(p, n);
@*/
